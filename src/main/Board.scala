import java.lang.IllegalArgumentException

/*
      a             b             c
    1 ⚫――――――――――――⚫――――――――――――⚫
      │   ⚫――――――――⚫――――――――⚫   │ 1
      │   │   ⚫――――⚫――――⚫   │ 2   │
      │   │   │            │ 3  │   │
    2 ⚫――⚫――⚫          ⚫――⚫――⚫
      │   │   │            │   │   │
      │   │   ⚫――――⚫――――⚫   │   │
      │   ⚫――――――――⚫――――――――⚫   │
    3 ⚫――――――――――――⚫――――――――――――⚫
 */

// chess input notation: a11 a21, a11, a11 c33
case class Board(fields: List[Field]) {
  val endOfLine = sys.props("line.separator")
  val lineHeight = 1
  val barWidth = 4
  @throws(classOf[IllegalArgumentException])
  // auxiliary constructor that creates the initial board with its fields respectively
  def this(size: Int) = this(
    // check if board size is valid
    if (size < 3 || size % 2 == 0)
      throw new IllegalArgumentException(
        "Invalid board size. Should be uneven and greater than 2."
      )
    // construct initial fields
    else
      (for {
        ring <- 0 until size
        row <- 0 until size
        col <- 0 until size;
        // filter out inner fields
        if !(row > 0 && row < size - 1 && col > 0 && col < size - 1)
      } yield Field(col, row, ring, "")).toList
  )
  override def toString(): String = {
    def bar(width: Int = barWidth) = "―" * width
    def line(height: Int = lineHeight) = "│" * height
    // vertical board size/dimension
    val largestRow = fields.maxBy(_.y).y
    val upperSection = fields.filter(field => field.y == 0)
    val middleSection =
      fields.filter(field => field.y > 0 && field.y < largestRow)
    val lowerSection = fields.filter(field => field.y == largestRow)
    return ""
  }
  def fieldsDump = fields
    .map(field => s"(${field.x}, ${field.y}, ${field.ring})")
    .mkString(",")
}
