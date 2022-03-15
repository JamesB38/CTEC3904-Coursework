/**
  * Author: drs
  * Picture library
  */
package lib.picture

class Picture(css: List[List[Char]]) {
  import Picture._
  val depth: Int = css.length
  val width: Int = depth match {
    case 0 => 0
    case _ => (css map (_.length)).max
  }
  val text: List[List[Char]] = width match {
        case 0 => List()
        case _=> css map (_.padTo(width, Picture.space))
  }
  
  def isEmpty: Boolean = depth==0
  def notEmpty: Boolean = !isEmpty
  
  override def toString: String =
    text.map(_.foldRight("")(_+_)).mkString("\n")
  
  def map(f: Char => Char): Picture =
    Picture(text.map(_.map(f)))
  
  def /^/(that: Picture): Picture =
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else Picture(this.text ::: that.text)
    
  def |+|(that: Picture): Picture =
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else Picture((this.text, that.text).zipped.map(_:::_))

  def fixWidth(newWidth: Int, position: Int=LFT, fill: Char=space): Picture = {
      val pos: Int = Integer.min(Integer.max(position, 0), 100)
      val len: Int = Math.abs(newWidth - this.width)
      val leftWidth: Int = len * pos / 100
		  val rightWidth: Int = len - leftWidth
		  if (newWidth < 1)
		    empty
		  else if (newWidth > width)
		    box(depth, leftWidth, fill) |+| this |+| box(depth, rightWidth, fill)
		  else
		    Picture(text map (_.take(newWidth+leftWidth)) map (_.drop(leftWidth)))
  }
    
  def fixDepth(newDepth: Int, position: Int=TOP, fill: Char=space): Picture = {
      val pos: Int = Integer.min(Integer.max(position, 0), 100)
      val len: Int = Math.abs(newDepth - depth)
      val topDepth: Int = len * pos / 100
		  val botDepth: Int = len - topDepth
		  if (newDepth < 1)
		    empty
		  else if (newDepth > depth)
		    box(topDepth, width, fill) /^/ this /^/ box(botDepth, width, fill)
		  else
		    Picture(text take (newDepth+topDepth) drop (topDepth))
  }

  def ^(that: Picture, position: Int = TOP, fill: Char = space): Picture =
    if (this.isEmpty)
      that
    else if (that.isEmpty)
      this
    else if (this.width < that.width)
      this.fixWidth(that.width, position, fill) /^/ that
    else
      this /^/ that.fixWidth(this.width, position, fill)

  def +(that: Picture, position: Int = TOP, fill: Char = space): Picture =
    if (this.isEmpty)
      that
    else if (that.isEmpty)
      this
    else if (this.depth < that.depth)
      this.fixDepth(that.depth, position, fill) |+| that
    else
      this |+| that.fixDepth(this.depth, position, fill)
  
  def above(that: Picture): Picture  = this ^ that
  def beside(that: Picture): Picture = this + that
  
  def transpose: Picture = Picture(text.transpose)
  def reflectH:  Picture = Picture(text.reverse)
  def reflectV:  Picture = Picture(text map (_.reverse))
  
  def rotate(quadrants: Int): Picture = quadrants%4 match {
		case 1 => this.transpose.reflectV
		case 2 => this.reflectH.reflectV
		case 3 => this.transpose.reflectH
		case _ => this
  }
    
  def borderL(fill: Char): Picture = box(depth,1,fill) + this
  def borderR(fill: Char): Picture = this + box(depth,1,fill)
  def borderT(fill: Char): Picture = box(1,width,fill) ^ this
  def borderB(fill: Char): Picture = this ^ box(1,width,fill)
  def border(fill:  Char): Picture = this.borderT(fill).borderB(fill).borderL(fill).borderR(fill)
  def frameL: Picture = this.borderL(vert)
  def frameR: Picture = this.borderR(vert)
  def frameT: Picture = this.borderT(horiz)
  def frameB: Picture = this.borderB(horiz)
  def frame:  Picture = this.frameL.frameR.frameT.frameB
}

object Picture {
  val space: Char = ' '
  val horiz: Char = '-'
  val vert:  Char = '|'
  val TOP: Int =   0
  val MID: Int =  50
  val BOT: Int = 100
  val LFT: Int =   0
  val CTR: Int =  50
  val RGT: Int = 100
  
  def apply(): Picture = new Picture(List())
  
  def apply(c: Char): Picture = new Picture(List(List(c)))
  
  def apply(css: List[List[Char]]): Picture = new Picture(css)
  
  def apply(lines: Array[String]): Picture = Picture(lines.toList map (_.toList))
  
  def apply(text: String, delim: Char): Picture = Picture(text.split(delim))
  def apply(text: String): Picture = Picture(text,'\n')
  
  def empty: Picture = apply()
  
  def box(d: Int, w: Int, c: Char): Picture =
    if (d <= 0 || w <= 0)
      empty
    else {
      val line = (1 to w).toList map (_ => c)
      val lines = (1 to d).toList map (_ => line)
      Picture(lines)
    }
   
  def stack(pictures: List[Picture], position: Int = LFT, fill: Char = space): Picture =
    pictures.foldRight(empty)((p,q) => p^(q,position,fill))
    
  def spread(pictures: List[Picture], position: Int = TOP, fill: Char = space): Picture =
    pictures.foldRight(empty)((p,q) => p+(q,position,fill))
    
  def stack(pictures: java.util.ArrayList[Picture]): Picture =
    stack(pictures.toArray.toList.asInstanceOf[List[Picture]])
      
  def spread(pictures: java.util.ArrayList[Picture]): Picture =
    spread(pictures.toArray.toList.asInstanceOf[List[Picture]])
    
  def maxWidth(pictures: List[Picture]): Int = (pictures map (_.width)).max
  def maxDepth(pictures: List[Picture]): Int = (pictures map (_.depth)).max
   
  def normaliseCol(pictures: List[Picture], position: Int = LFT, fill: Char = space): List[Picture] = {
   val mw = if (pictures.isEmpty) 0 else maxWidth(pictures)
   pictures map (p => p.fixWidth(mw, position, fill))
  }
   
 def normaliseRow(pictures: List[Picture], position: Int = TOP, fill: Char = space): List[Picture] = {
   val md = if (pictures.isEmpty) 0 else maxDepth(pictures)
   pictures map (p => p.fixDepth(md, position, fill))
 }
   
 private def makeTableCol(pictures: List[Picture]) = stack(pictures map (_.frameT)).frameB
 private def makeTableRow(pictures: List[Picture]) = spread(pictures map (_.frameL)).frameR
 
 def makeTableFromCols(rows: List[List[Picture]], vAdjust: Int = TOP,
                                                  hAdjust: Int = LFT, fill: Char = space): Picture = {
   val normalised = {
     val normCols = rows map (normaliseCol(_,hAdjust,fill))
     normCols.transpose.map(normaliseRow(_,vAdjust,fill)).transpose
   }
   makeTableRow(normalised map makeTableCol)   
 }
   
 def makeTableFromRows(rows: List[List[Picture]], vAdjust: Int = TOP,
                                                  hAdjust: Int = LFT, fill: Char = space): Picture = {
   val normalised = {
     val normRows = rows map (normaliseRow(_,vAdjust,fill))
     normRows.transpose.map(normaliseCol(_,hAdjust,fill)).transpose
   }
   makeTableCol(normalised map makeTableRow)   
 }

}
