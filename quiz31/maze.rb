#!/usr/bin/ruby

class Array ; def random ; return nil if length.zero? ; entries[rand(length)] ; end ; end

class Window
  require 'highline/system_extensions'
  attr_reader :x, :y, :height, :width, :window
  def initialize options = {}
    @height, @width = options[:height], options[:width]
    @y = options[:y] || options[:top]  || 0
    @x = options[:x] || options[:left] || 0
    @window = options[:window] || Ncurses::WINDOW.new(height, width, top, left)
  end
  alias :top  :y
  alias :left :x
  def clear ; window.clear ; end
  def print_xy str, x, y
    Ncurses.mvwprintw window, y, x, str
  end
  def print_yx str, y, x
    print_xy str, x, y
  end
  def refresh ; window.refresh ; end
  def pos ; [x, y] ; end
  def print_border color
    top    = '/'  + ('-' * (width - 2)) + '\\'
    mid    = '|'  + (' ' * (width - 2)) + '|'
    bottom = '\\' + ('-' * (width - 2)) + '/'
    print_yx(top   ,      0    , 0)
    print_yx(bottom, height - 1, 0)
    (height - 2).times {|y|
      print_yx mid ,      y + 1, 0
    }
  end
  def scroll n = 1
#    window.scroll n
    Ncurses.wscrl window, n
  end
  def print str
    scroll
    print_xy str, 0, height - 1
  end
  def scroll_ok state = true
    window.scrollok state
  end
  def self.stdscr
    width, height = Ncurses.COLS, Ncurses.LINES
    new :height => height, :width => width, :window => Ncurses.stdscr
  end
end

class Cell

	def initialize walls=nil
		@walls= {}
		@neighbors= {}
    @walked_on=false
    @highlight=false
	end

	attr_reader :neighbors, :walls, :highlight
  attr_accessor :contents

  def walked_on? ; @walked_on ; end

  def set_highlight state = true ; @highlight = state ; end
  def unset_highlight ; set_highlight false ; end
  def highlight? ; @highlight ; end

	def set_wall direction, state = true, both = true
    direction = direction.to_sym
    neighbors[direction].set_wall reverse_dir(direction), false, state if both
    walls[direction] = state
	end

	def unset_wall direction, both = true
    set_wall direction, false, both
	end

  def passable? direction, both = false
    direction = direction.to_sym
    return false unless neighbors[direction]
    return false unless neighbors[direction].passable? reverse_dir(direction), false if both
    !walls[direction] # if there isn't a wall, you're free to go
  end

  def reverse_dir direction
    direction = direction.to_sym
    @reverse_map ||= begin
      directions = [%w(east west), %w(north south), %w(up down), %w(in out)].collect {|a| a.collect &:to_sym }
      directions.inject({}) {|h,a| h[a.first] = a.last ; h[a.last] = a.first ; h }
    end
    @reverse_map[direction] || "reverse_of_#{direction}".to_sym
  end

	def add_neighbor direction, neighbor, reverse = false
    direction = direction.to_sym
    neighbor.add_neighbor direction, self, true unless reverse
    direction = reverse_dir(direction) if reverse
		neighbors[direction] = neighbor
    walls[direction]     = true
	end

	def del_neighbor direction, reverse = false
    direction = direction.to_sym
    if reverse
      direction = reverse_dir(direction)
    else
      raise "No such neighbor - #{direction} from #{inspect}" unless neighbors[direction]
      neighbors[direction].del_neighbor direction, true
    end
		[neighbors, walls].each {|h| h.delete direction }
	end

  def visited?
    walls.any? {|dir,wall| !wall }
  end

  def unvisited_neighbors
    neighbors.select {|dir,cell| !cell.visited? }
  end

  def not_walked_on_neighbors
    neighbors.select {|dir,cell| self.passable?(dir) and !cell.walked_on? }
  end

	def dump
		[neighbors,walls]
	end

  def to_s
    "#{neighbors.length} neighbors - #{walls.inspect}, V:#{visited?}, W:#{walked_on?}, H:#{highlight?}, C:#{contents}"
  end

  def inspect
    "Cell(##{object_id.to_s(16)} #{to_s}"
  end

  def display options = {}
    wall             = options[:wall]      || '#'
    highlight_char   = options[:highlight] || 'X'
    walked           = options[:walked]    || '.'
    open             = options[:open]      || ' '
    north_south_open = options[:north_south_open]
    east_west_open   = options[:east_west_open]
    highlight_char = highlight.to_s[0..0] if highlight? && highlight != true # If it's not simply true, use it

    base_floor = walked_on? ? walked : open
    floor = (highlight_char if highlight?) || (contents[0..0] if contents) || (wall if !visited?) || base_floor

    return [[floor]] if options[:cell_display_size] == 1

    north, south, east, west = %w(north south east west).collect {|dir| passable?(dir, false) ? base_floor  : wall }
    nw, ne, se, sw =           %w(nw ne se sw          ).collect {|dir| options["#{dir}_wall".to_sym]      || wall }

    if options[:darkness] && !walked_on?
      north = south = east = west = nw = ne = se = sw = ' '
      floor = ' ' unless contents
    end

    output =  [ [nw  , north, ne  ],
                [west, floor, east],
                [sw  , south, se  ] ]

    return output if options[:cell_display_size] == 3

    output[0...-1].collect {|a| a[0...-1] } # 2x2 output
  end

  def display_curses top, left, options = {}
    window = options[:window] || Window.stdscr
    content = display(options)
    content.each_with_index {|line, row_offset| window.print_yx line.join, top + row_offset, left }
    :curses
  end

  def walk_on
    @walked_on=true
  end
end

class Maze

  attr_reader :board, :length, :width, :highlighted_cell, :generated, :start_cell, :end_cell, :move_log

	def initialize options = {}
    setup_board options
  end

  def wipe_designations
    @highlighted_cell = @start_cell = @end_cell = @generated = @solved = @move_log = nil
  end

  def setup_board options = {}
    l = options[:length] || length
    w = options[:width]  || width
    raise "length and width must be fixnums greater than zero" unless [l, w].all? {|n| n.respond_to?(:to_i) && !n.to_i.zero? }
    @length, @width = l.to_i, w.to_i
    wipe_designations
    circular = options[:circular]
		@board=[[]]
		(0...length).each {|l|
			@board[l] ||=[]
			(0...width).each{|w|
        if circular
  				@board[l][w]=nil
          d = ((l - length / 2) ** 2 + (w - width / 2) ** 2)
          dim = [length, width].min / 2.to_f
          next unless d < ((    dim / 1) ** 2 - 2)
          next unless d > ((    dim / 3) ** 2 - 1)
        end
				@board[l][w]=cell=Cell.new
        oc = @board[l-1][w]
        oc.add_neighbor(:south, cell) unless l == 0 if oc
				oc = @board[l][w-1]
        oc.add_neighbor(:east,  cell) unless w == 0 if oc
			}
		}
		@board
	end

  def location_of target
    return nil unless target
    board.each_with_index {|row,  y|
      row.each_with_index {|cell, x|
        return [y,x] if cell == target
      }
    }
    nil
  end

  def set_highlight cell
    @highlighted_cell.unset_highlight if @highlighted_cell
    return if (@highlighted_cell = cell).nil?
    @highlighted_cell.set_highlight
  end

  def set_start_cell cell
    start_cell.contents = nil if start_cell
    return if (@start_cell = cell).nil?
    start_cell.contents = 'Start'
  end

  def set_end_cell cell
    end_cell.contents = nil if end_cell
    return if (@end_cell = cell).nil?
    end_cell.contents = 'End'
  end

  def random_cell
    cell = nil
    cells = board.flatten.compact
    loop do
      return nil unless cell = cells.random
      cells.delete cell
      next unless yield(cell) if block_given?
      return cell
    end
  end

  def generate options = {}
    watch = options[:watch]
    delay = options[:delay].to_f || 0.2
    return false if generated
    starting_cell = random_cell {|cell| !cell.visited? }
    list = [ starting_cell ]
    begin
      cell = list.last
      begin ; set_highlight cell ; display options ; sleep delay ; end if watch
      unvisited = cell.unvisited_neighbors
      if unvisited.empty?
        list.pop
      else
        dir, other = unvisited.random
        cell.unset_wall dir
        list << other
      end
    end while !list.empty?
    set_highlight nil if watch
    @generated = true
  end

  def stats
    return nil unless generated
    cells = board.flatten.compact
    num = cells.length
    branches = walked_on = branches_passed = unreachable = 0
    cells.each {|cell|
      exits = cell.walls.select {|d,state| !state }.length
      walked_on += 1 if cell.walked_on?
      branches_passed = (exits - 1) if branches_passed.zero?
      if exits > 2
        branches += (exits - 2)
        if cell.walked_on?
          branches_passed += (exits - 2)
        end
      end
      unreachable += 1 if !cell.visited? || exits == 0
    }
    [num, branches, (num / branches.to_f), walked_on, branches_passed, unreachable]
  end

  def solve options = {}
    watch = options[:watch]
    delay = options[:delay].to_f || 0.2
    return false unless generated
    starting_cell = random_cell
    crawl( starting_cell, 'not_walked_on_neighbors' ) {|cell, dir|
      cell.walk_on
      begin ; set_highlight cell ; display options ; sleep delay ; end if watch
    }
    set_highlight nil if watch
  end

  def crawl(starting_cell, get_neighbors)
    list = [ starting_cell ]
    begin
      cell=list.last
      neighbors= cell.send(get_neighbors.to_sym)
      if neighbors.empty?
        yield cell,nil
        list.pop
      else
        dir, other = neighbors.random
        yield cell, dir
        list << other
      end
    end while !list.empty?
  end

  def display_curses options = {}
    options = options.dup
    window = options[:window] || Window.stdscr
    cell_size = options[:cell_display_size] ||= 2
    wall_char = options[:wall] ||= '#'
    pad_top    = pad_left  = (cell_size < 2)
    pad_bottom = pad_right = (cell_size < 3)
    pad_horiz = (width  * cell_size) + (pad_left ? 1 : 0) + (pad_right  ? 1 : 0)
    pad_vert  = (length * cell_size) + (pad_top  ? 1 : 0) + (pad_bottom ? 1 : 0)
    pad_vert.times  {|y| window.print_yx(wall_char,        y    ,         0    ) } if pad_left
    pad_vert.times  {|y| window.print_yx(wall_char,        y    , pad_horiz - 1) } if pad_right
    pad_horiz.times {|x| window.print_yx(wall_char,        0    ,         x    ) } if pad_top
    pad_horiz.times {|x| window.print_yx(wall_char, pad_vert - 1,         x    ) } if pad_bottom
    y_offset = pad_top  ? 1 : 0
    x_offset = pad_left ? 1 : 0
    board.each_with_index {|row, y|
      dy = y * cell_size + y_offset
      row.each_with_index {|cell,x|
        dx = x * cell_size + x_offset
        cell.display_curses dy, dx, options if cell
      }
    }
    window.refresh
  end

  def display options = {}
    options = options.dup
    options[:darkness] = false if solved? # show the whole board once solved
    cell_size = options[:cell_display_size] ||= 2
    wall_char = options[:wall] ||= '#'
    fake = [[wall_char, wall_char], [wall_char, wall_char]]
    if options[:curses]
      display_curses options # requires curses to already be initialized
    else
      back_pad   = wall_char                   if (cell_size == 2)
      display_width = width * cell_size + (back_pad || '').length
      bottom_pad = (wall_char * display_width) if (cell_size == 2)
      top_pad    = bottom_pad                  if (cell_size == 3)
      print `clear`
      puts top_pad if top_pad
      board.each {|cells|
        rows = cells.inject([]) {|rows, cell|
          output = cell ? cell.display(options) : fake
          output.each_with_index {|crow,i| (rows[i] ||= []) << crow }
          rows
        }
        rows = rows.collect {|row| "#{row.join}#{back_pad}" }
        puts rows.collect {|row| row }
      }
      puts bottom_pad if bottom_pad
    end
    nil
  end

  def solved?
    @solved ||= begin
      true if highlighted_cell && highlighted_cell == end_cell # only return and nil, so as not to cache a false
    end
  end

  def move_to_cell cell
    unless solved?
      cell.walk_on # only store footprints while trying to solve the maze
      @move_log ||= []
      @move_log << cell
    end
    set_highlight cell
  end

  def move dir
    return false unless cell = highlighted_cell
    new = cell.neighbors[dir] if cell.passable?(dir,false)
    move_to_cell new if new
  end

  def self.run_command maze, options = {}
    str = begin
      puts 'Enter a ruby command:'
      cmd = $stdin.gets.strip # Readline.readline
      maze.instance_eval(cmd).inspect
    rescue Exception => e
      ["#{e.class}: #{e.message}",e.backtrace].join("\n\t")
    end
    str[0...2048]
  end

  require 'highline'  # make these options, the program should simply degrade
  require 'rbcurse'
  def self.test_for_highline ; @highline ||= begin ; require 'highline' ; true ; rescue LoadError ; false ; end ; end
  def self.test_for_curses   ; @curses   ||= begin ; require 'rbcurse'  ; true ; rescue LoadError ; false ; end ; end
  def self.read_a_char options = {}
    if test_for_curses
      key = Ncurses.getch
      -1 != key ? Ncurses.keyname(key) : nil
    elsif test_for_highline
      HighLine::SystemExtensions.get_character.chr
    else
      raise "No unbuffered input available"
    end
  end
  def self.get_input options = {}
    input = if options[:curses]
      response = c = read_a_char(options)
      if c == "\e"
        loop do
          response += c = read_a_char(options)
          break if c =~ /[a-zA-Z]/
        end
      end
      response
    else
      $stdin.gets.strip
    end
  end

  def self.border_and_return_sub window
    x, y = window.pos
    h, w = window.height, window.width
    window.print_border 6
    window.refresh
    inner = Window.new :height => h - 2, :width => w - 2, :y => y + 1, :x => x + 1
    inner.scroll_ok
    inner
  end

  def self.setup_windows options = {}
    width, height = Ncurses.COLS, Ncurses.LINES
    cli_height = 6
    board = Window.new(:top =>                    0 , :left => 0, :height => (height - cli_height), :width => width)
    cli   = Window.new(:top => (height - cli_height), :left => 0, :height =>           cli_height , :width => width)
    [board, cli].collect {|win| win.refresh ; border_and_return_sub win }
  end

  def self.play maze = nil, options = {}
    options, maze = maze, nil if maze.is_a?(Hash)
    curses = options.member?(:curses) ? options[:curses] : false

    catch(:quit) do
      begin
        out = if curses
          VER::start_ncurses
#          Ncurses.initscr
#          Ncurses.nl
#          Ncurses.noecho
#          Ncurses.curs_set(0)
#          Ncurses.stdscr.nodelay(true)
#          Ncurses.timeout(0)
          board, cli = setup_windows options
          options[:window] = board
          lambda {|str| cli.print str ; cli.refresh }
        else
          lambda {|str| puts str }
        end
        options[:length] ||= 11
        options[:width]  ||= 11
        options[:watch] ||= true
        options[:delay] ||= 0.03
        maze = Maze.new options unless maze

        command = nil
        result = nil
        loop do
          if maze.generated && !maze.start_cell && !maze.end_cell
            maze.set_start_cell maze.random_cell
            maze.set_end_cell   maze.random_cell {|cell| cell != maze.start_cell }
            maze.move_to_cell maze.start_cell
          end
          maze.display options
          out.call "Congratulations, you have navigated the maze" if maze.solved?
          out.call "Last command: #{command}" if command
          out.call result if result
          result = nil
          out.call "Command: "
          command = get_input options
          case command
            when /^q/ ; throw :quit
            when /^o/ ; out.call options.inspect
            when /^w/ ; options[:watch] = !options[:watch] ; result = "Watch is #{options[:watch]}"
            when /^n/ ; maze.setup_board options
            when /^c/ ; options[:circular] = !options[:circular]
            when /^g/ ; maze.generate(options)
            when /^s/ ; maze.solve(options)
            when /^;/ ; out.call run_command(maze, options)
            when /^D/ ; options[:darkness] = !options[:darkness]
            when /^(\d+)\s?,\s?(\d+)$/ ; maze = Maze.new options.merge!({:length => $1, :width => $2})
            when /^([123])$/ ; options[:cell_display_size] = $1.to_i
            when /^d(elay)?(=|\s?)([0-9.]+)/ ; options[:delay] = $3.to_f ; result = "Delay is #{options[:delay]}"
            when /^([ijkl])/ ; maze.move dirs = {'i' => :north, 'j' => :west, 'k' => :south, 'l' => :east}[$1] if maze.highlighted_cell
            when /^f/ ; next unless maze.highlighted_cell ; choices = maze.highlighted_cell.not_walked_on_neighbors ; next unless choices.length == 1 ; maze.move choices.first.first
            when /^r/ ; load __FILE__
            when /^u/
                next unless ml = maze.move_log
                ml = ml.dup
                move = ml.pop
                loop do
                    break if move.not_walked_on_neighbors.length > 0
                    break unless move = ml.pop
                end
                next unless move
                maze.move_to_cell move
            end
          end
          maze
      ensure
        if options[:curses]
          VER::stop_ncurses
#          Ncurses.curs_set(1) # cbreak / nocbreak
#          Ncurses.endwin()
        end
      end
    end
    maze
  end

  def self.cli args
    options = {}
    args.each {|arg|
      next unless arg =~ /^-+([^=]*)(=(.*))?/ # key=val stored in $1 and $3
      args.delete arg                         # all non-numeric args are parsed and removed
      key, value = $1, $3
      case key
        when /^c(irc(le|ular)?)?$/ ; options[:circular] = true
        else ; puts "Unknown option #{arg} - parsed as #{key.inspect} = #{value.inspect}" ; exit
      end
    }
    options[:length], options[:width] = args.collect(&:to_i)
    play options
  end
end

if $0 == __FILE__
  Maze.cli ARGV
end
