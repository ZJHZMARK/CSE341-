# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  All_My_Pieces =[[[[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]],
                  [[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]],
                  rotations([[0, 0], [1, 0], [0, 1]]),
                  rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 0]])] + All_Pieces


  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

end


class MyBoard < Board
  def initialize (game)
    super(game)
    @cheated = false
    @current_block = MyPiece.next_piece(self)
  end

  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size - 1).each {|index|
      current = locations[index];
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] =
          @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat
    if !@cheated and score > 100
      @cheated = true
      @score -= 100
    end
  end

  def next_piece

    if @cheated
      @cheated = false
      @current_block = MyPiece.new([[[0, 0]]].sample, self)
    else
      @current_block = MyPiece.next_piece(self)
    end
    @check = false
    @current_pos = nil
  end

end

class MyTetris < Tetris

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    @root.bind('u', proc {@board.rotate_180_degrees})
    @root.bind('c', proc {@board.cheat})
    super

  end

end








