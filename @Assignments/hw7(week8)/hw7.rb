# University of Washington, Programming Languages, Homework 7, hw7.rb 
# (See also ML code)
# a little language for 2D geometry objects
# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for 
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but
#  these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by 
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction
# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression  
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue 
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2) 
    (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2) 
    real_close(x1,x2) && real_close(y1,y2)
  end
  def real_close_or_smaller(r1,r2)
    real_close(r1,r2) or r1 < r2
  end
  def real_close_or_greater(r1,r2)
    real_close(r1,r2) or r1 > r2
  end

  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2) 
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m,b)
    end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  def eval_prog env
    self
  end
  
  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)

  # Note: no initialize method only because there is nothing it needs
  # to do
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self
  end
  def intersectLine line
    self
  end
  def intersectVerticalLine vline
    self
  end
  def intersectWithSegmentAsLineResult seg
    self
  end
end

class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x, :y

  def initialize(x,y)
    @x = x
    @y = y
  end

  def preprocess_prog
    self
  end

  def shift(dx, dy)
    Point.new(x + dx, y + dy)
  end

  def intersect otherVal
    otherVal.intersectPoint(self)
  end
  
  def intersectPoint p
    if real_close_point(x, y, p.x, p.y) then self else NoPoints.new end
  end
  
  def intersectLine line
    p = Point.new(x, line.m * x + line.b)
    if real_close_point(x, y, p.x, p.y) then self else NoPoints.new end
  end
  
  def intersectVerticalLine vline
    if real_close(x, vline.x) then self else NoPoints.new end
  end
  
  def intersectWithSegmentAsLineResult seg
    if (real_close_or_greater(x, seg.x1) and
        real_close_or_greater(y, seg.y1) and
        real_close_or_smaller(x, seg.x2) and
        real_close_or_smaller(y, seg.y2))
    then self else NoPoints.new end
  end
end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b 
  def initialize(m,b)
    @m = m
    @b = b
  end

  def preprocess_prog
    self
  end

  def shift(dx, dy)
    Line.new(m, b + dy - m * dx)
  end

  def intersect(otherVal)
    otherVal.intersectLine(self)
  end

  def intersectPoint p
    possiblePoint = Point.new(p.x, b + m * p.x)
    if real_close_point(p, possiblePoint) then p else NoPoints.new end
  end

  def intersectLine line
    if real_close(line.m, m) then
      if real_close(line.b, b) then self else NoPoints.new end
    else
      thisX = (line.b - b)/(m - line.m)
      thisY = thisX * m + b
      
      Point.new(thisX, thisY)
    end
  end
  
  def intersectVerticalLine vline
    Point.new(vline.x, vline.x * m + b)
  end

  def intersectWithSegmentAsLineResult seg
    seg
  end
end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x
  def initialize x
    @x = x
  end

  def preprocess_prog
    self
  end

  def shift(dx, dy)
    VerticalLine.new(x + dx)
  end

  def intersect(otherVal)
    otherVal.intersectVerticalLine(self)
  end

  def intersectPoint p
    if real_close(p.x, x) then p else NoPoints.new end
  end
  
  def intersectLine line
    Point.new(x, line.m * x + line.b)
  end
  def intersectVerticalLine vline
    if real_close(vline.x, x) then self else NoPoints.new end
  end
  def intersectWithSegmentAsLineResult seg
    seg
  end
end

class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and 
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  def preprocess_prog
    if real_close(x1,x2)
      if real_close(y1,y2)
        Point.new(x1,y1)
      else
        if y1 > y2
          LineSegment.new(x2,y2,x1,y1)
        else
          self
        end
      end
    else
      if x1 > x2
        LineSegment.new(x2,y2,x1,y1)
      else
        self
      end
    end
  end

  def shift(dx, dy)
    LineSegment.new(x1 + dx, y1 + dy,
                    x2 + dx, y2 + dy)
  end
  
  def intersect(otherVal)
    otherVal.intersectLineSegment(self)
  end
  
  def intersectPoint p
    p.intersectSegmentAsLineResult(self)
  end
  
  def intersectLine line
    line.intersectSegmentAsLineResult(self)
  end
  
  def intersectVerticalLine vline
    vline.intersectSegmentAsLineResult(self)
  end
  
  def intersectWithSegmentAsLineResult seg
    if x1 > seg.x2 or x2 < seg.x1 then NoPoints.new
    elsif real_close(x1, seg.x2) then Point.new(x1,y1)
    elsif real_close(x2, seg.x1) then Point.new(x2,y2)
    else
      if x1 < seg.x1 then
        fp = Point.new(x1,y1)
      else
        fp = Point.new(seg.x1, seg.y1)
      end

      if x2 < seg.x2 then
        sp = Point.new(x2,y2)
      else
        sp = Point.new(seg.x2, seg.y2)
      end

      LineSegment.new(fp.x, fp.y, sp.x, sp.y)
    end
  end
end

class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end
  
  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end
  
  def eval_prog env
    v1 = @e1.eval_prog(env)
    v2 = @e2.eval_prog(env)
    
    v1.intersect(v2)
  end
end

class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    Let.new(@s, @e1.preprocess_prog, @e2.preprocess_prog)
  end

  def eval_prog env
    @e2.eval_prog([[@s, @e1.eval_prog(env)]] + env)
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end

  def preprocess_prog
    self
  end

  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end

  def preprocess_prog
    Shift.new(@dx,@dy,@e.preprocess_prog)
  end

  def eval_prog env
    @e.eval_prog(env)
      .shift(@dx, @dy)
  end
end
