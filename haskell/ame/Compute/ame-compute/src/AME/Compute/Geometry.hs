module AME.Compute.Geometry (
  Shape(..),
  Cartesian2(..),
  linePointGradient,
  linePointPoint,
  lineInterceptGradient,
  circleCenterRadius,
  circleCenterPoint,
  equationOf,
  intersectionOf
) where

import AME.Compute.Expr
import AME.Compute.Error
import AME.Compute.Solver.Polynomial
import AME.Compute.Solver
import AME.Compute.Simplify

-- Variables parameterising a cartesian equation in two dimensions
data Cartesian2 = X | Y deriving (Eq, Show, Ord)

--Represents lines as a point and a gradient
--and circles a center and radius
data Shape t = Circle (t, t) t
             | Line (t, t) t
             deriving (Show, Eq)

--an intercept of c is just the point (0, c)
--so put it straight into our representation
lineInterceptGradient :: Num t => t -> t -> Shape t
lineInterceptGradient c m = Line (0, c) m

linePointGradient :: (t, t) -> t -> Shape t
linePointGradient = Line

--the line between two points simplify starts at either point
--and has the gradient ∆y/∆x
linePointPoint :: Fractional t => (t, t) -> (t, t) -> Shape t
linePointPoint (x1, y1) (x2, y2) = linePointGradient (x1, y1) $ (y2 - y1) / (x2 - x1)

circleCenterRadius :: (t, t) -> t -> Shape t
circleCenterRadius = Circle

--Work out the radius by computing the distance from the center to the point
--with pythagoras
circleCenterPoint :: Floating t => (t, t) -> (t, t) -> Shape t
circleCenterPoint (x1, y1) (x2, y2) = Circle (x1, y1) (sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2)

--The equation of a circle is given by x^2 + y^2 = r^2
--and for a line is given by (y - y1) = m(x - x1) and hence y = mx - mx1 + y1
equationOf :: Num t => Shape t -> Equation Cartesian2 t
equationOf (Circle (cx, cy) r) = Equation ((Var X - Const cx)^2 + (Var Y - Const cy)^2)  ((Const r)^2)
equationOf (Line (ox, oy) m) = Equation (Var Y) ((Const m) * (Var X - Const ox) + (Const oy))

intersectionOf :: (Ord t, Floating t, IsIntegral t) => Shape t -> Shape t -> Compute [(t, t)]
intersectionOf (Line (ax, ay) am) (Line (bx, by) bm) = if am == bm
    --if they are parallel
    then if (ax == bx) && (ay == by)
        then throwError IntersectsEverywhere
        --they're either the same
        else return []
        --or don't intersect
    else return [(x, y)]
    --otherwise find the intersection point
    where x = (bm*bx - am*ax + ay - by)/(bm - am)
          y = am * (x - ax) + ay
intersectionOf (Line (px, py) m) (Circle (cx, cy) r) = return $ map (\t -> (px + t, py + m*t)) ts
--trying to find the intersection of a line and a circle results in a quadratic in the relative
--distances between the points
    where ts = solveQuadratic (1 + m*m) (2*(dx + m*dy)) (dx*dx + dy*dy - r*r)
          dx = px - cx
          dy = py - cy
intersectionOf a@(Circle _ _) b@(Line _ _) = intersectionOf b a
intersectionOf c@(Circle (ax, ay) ar) (Circle (bx, by) br) | ay /= by = do
--Two intersect two circles that are not in the same horizontal line
    lhs' <- substitute Y sub lhs
    xs <- solve X $ Equation lhs' rhs
    --make a substituteion and solve
    ys <- mapM (>>= evaluate) $ map (\x -> substitute X (Const x) sub) xs
    --then evaluate those x values to determine the whole coordinates
    return $ zip xs ys
    where sub = Const (((br*br - ar*ar) - (bx*bx - ax*ax) - (by*by - ay*ay)) / (2*(ay - by))) + Mul [Const ((bx - ax)/(ay - by)), Var X]
          --the substitution (br^2 - ar^2) - (bx^2 - ax^2) - (ay^2 - ay^2) / 2*(ay - by) + (bx - ax)/(ay - by)x
          --transforms the equation of a circle into an equation in one variable.
          Equation lhs rhs = equationOf c
--same thing but subbing for X instead of Y
intersectionOf c@(Circle (ax, ay) ar) (Circle (bx, by) br) | ax /= bx = do
    lhs' <- substitute Y sub lhs
    ys <- solve X $ Equation lhs' rhs
    xs <- mapM (>>= evaluate) $ map (\y -> substitute Y (Const y) sub) ys
    return $ zip xs ys
    where sub = Const (((br*br - ar*ar) - (bx*bx - ax*ax) - (by*by - ay*ay)) / (2*(ax - bx))) + Mul [Const ((by - ay)/(ax - bx)), Var Y]
          Equation lhs rhs = equationOf c
--otherwise they intersect everywhere
intersectionOf c@(Circle (ax, ay) ar) (Circle (bx, by) br) | (ay == by) && (ax == bx) && (ar == br) = throwError IntersectsEverywhere
