module Main (main) where
vec_add :: Vec -> Vec -> Vec
(x1_anB, y1_anC) `vec_add` (x2_anD, y2_anE)
  = (x1_anB + x2_anD, y1_anC + y2_anE)
vec_sub :: Vec -> Vec -> Vec
(x1_aoD, y1_aoE) `vec_sub` (x2_aoF, y2_aoG)
  = (x1_aoD - x2_aoF, y1_aoE - y2_aoG)
scale_vec2 :: Vec -> Int -> Int -> Vec
scale_vec2 (x_aoH, y_aoI) a_aoJ b_aoK
  = ((x_aoH * a_aoJ) `div` b_aoK, (y_aoI * a_aoJ) `div` b_aoK)
p_tile :: [(Int, Int, Int, Int)]
q_tile :: [(Int, Int, Int, Int)]
r_tile :: [(Int, Int, Int, Int)]
s_tile :: [(Int, Int, Int, Int)]
p_tile
  = [(0, 3, 3, 4), (3, 4, 0, 8), (0, 8, 0, 3), (6, 0, 4, 4),
     (4, 5, 4, 10), (4, 10, 7, 6), (7, 6, 4, 5), (11, 0, 10, 4),
     (10, 4, 9, 6), (9, 6, 8, 8), (8, 8, 4, 13), (4, 13, 0, 16),
     (0, 16, 6, 15), (6, 15, 8, 16), (8, 16, 12, 12), (12, 12, 16, 12),
     (10, 16, 12, 14), (12, 14, 16, 13), (12, 16, 13, 15),
     (13, 15, 16, 14), (14, 16, 16, 15), (8, 12, 16, 10), (8, 8, 12, 9),
     (12, 9, 16, 8), (9, 6, 12, 7), (12, 7, 16, 6), (10, 4, 13, 5),
     (13, 5, 16, 4), (11, 0, 14, 2), (14, 2, 16, 2)]
q_tile
  = [(0, 8, 4, 7), (4, 7, 6, 7), (6, 7, 8, 8), (8, 8, 12, 10),
     (12, 10, 16, 16), (0, 12, 3, 13), (3, 13, 5, 14), (5, 14, 7, 15),
     (7, 15, 8, 16), (2, 16, 3, 13), (4, 16, 5, 14), (6, 16, 7, 15),
     (0, 10, 7, 11), (9, 13, 8, 15), (8, 15, 11, 15), (11, 15, 9, 13),
     (10, 10, 8, 12), (8, 12, 12, 12), (12, 12, 10, 10), (2, 0, 4, 5),
     (4, 5, 4, 7), (4, 0, 6, 5), (6, 5, 6, 7), (6, 0, 8, 5),
     (8, 5, 8, 8), (10, 0, 14, 11), (12, 0, 13, 4), (13, 4, 16, 8),
     (16, 8, 15, 10), (15, 10, 16, 16), (13, 0, 16, 6), (14, 0, 16, 4),
     (15, 0, 16, 2), (0, 0, 8, 0), (12, 0, 16, 0), (0, 0, 0, 8),
     (0, 12, 0, 16)]
r_tile
  = [(0, 0, 8, 8), (12, 12, 16, 16), (0, 4, 5, 10), (0, 8, 2, 12),
     (0, 12, 1, 14), (16, 6, 11, 10), (11, 10, 6, 16), (16, 4, 14, 6),
     (14, 6, 8, 8), (8, 8, 5, 10), (5, 10, 2, 12), (2, 12, 0, 16),
     (16, 8, 12, 12), (12, 12, 11, 16), (1, 1, 4, 0), (2, 2, 8, 0),
     (3, 3, 8, 2), (8, 2, 12, 0), (5, 5, 12, 3), (12, 3, 16, 0),
     (11, 16, 12, 12), (12, 12, 16, 8), (13, 13, 16, 10),
     (14, 14, 16, 12), (15, 15, 16, 14)]
s_tile
  = [(0, 0, 4, 2), (4, 2, 8, 2), (8, 2, 16, 0), (0, 4, 2, 1),
     (0, 6, 7, 4), (0, 8, 8, 6), (0, 10, 7, 8), (0, 12, 7, 10),
     (0, 14, 7, 13), (13, 13, 16, 14), (14, 11, 16, 12),
     (15, 9, 16, 10), (16, 0, 10, 4), (10, 4, 8, 6), (8, 6, 7, 8),
     (7, 8, 7, 13), (7, 13, 8, 16), (12, 16, 13, 13), (13, 13, 14, 11),
     (14, 11, 15, 9), (15, 9, 16, 8), (10, 16, 11, 10), (12, 4, 10, 6),
     (10, 6, 12, 7), (12, 7, 12, 4), (15, 5, 13, 7), (13, 7, 15, 8),
     (15, 8, 15, 5)]
nil a_axU b_axV c_axW = []
grid ::
  Int
  -> Int
     -> [Line_segment]
        -> Vec -> Vec -> Vec -> [Line_segment]
grid m_axX n_axY segments_axZ a_ay0 b_ay1 c_ay2
  = [tup2
       (a_ay0 `vec_add` (scale_vec2 b_ay1 x0_ay3 m_axX)
        `vec_add` (scale_vec2 c_ay2 y0_ay4 n_axY))
       (a_ay0 `vec_add` (scale_vec2 b_ay1 x1_ay5 m_axX)
        `vec_add` (scale_vec2 c_ay2 y1_ay6 n_axY)) |
       (x0_ay3, y0_ay4, x1_ay5, y1_ay6) <- segments_axZ]
rot p_ay7 a_ay8 b_ay9 c_aya
  = p_ay7
      (a_ay8 `vec_add` b_ay9) c_aya ((0, 0) `vec_sub` b_ay9)
beside m_ayb n_ayc p_ayd q_aye a_ayf b_ayg c_ayh
  = p_ayd a_ayf (scale_vec2 b_ayg m_ayb (m_ayb + n_ayc)) c_ayh
    ++
      q_aye
        (a_ayf
         `vec_add` (scale_vec2 b_ayg m_ayb (m_ayb + n_ayc)))
        (scale_vec2 b_ayg n_ayc (n_ayc + m_ayb))
        c_ayh
above m_ayi n_ayj p_ayk q_ayl a_aym b_ayn c_ayo
  = p_ayk
      (a_aym
       `vec_add` (scale_vec2 c_ayo n_ayj (m_ayi + n_ayj)))
      b_ayn
      (scale_vec2 c_ayo m_ayi (n_ayj + m_ayi))
    ++ q_ayl a_aym b_ayn (scale_vec2 c_ayo n_ayj (m_ayi + n_ayj))
tup2 ::
  (a_anx, b_any) -> (c_anz, d_anA) -> (a_anx, b_any, c_anz, d_anA)
tup2 (a_ayp, b_ayq) (c_ayr, d_ays)
  = (a_ayp, b_ayq, c_ayr, d_ays)
tile_to_grid = grid 16 16
p = tile_to_grid p_tile
q = tile_to_grid q_tile
r = tile_to_grid r_tile
s = tile_to_grid s_tile
quartet a_ayt b_ayu c_ayv d_ayw
  = above
      1 1 (beside 1 1 a_ayt b_ayu) (beside 1 1 c_ayv d_ayw)
t = quartet p q r s
cycle' p1_ayx
  = quartet
      p1_ayx
      (rot (rot (rot p1_ayx)))
      (rot p1_ayx)
      (rot (rot p1_ayx))
u = cycle' (rot q)
side1
  = quartet nil nil (rot t) t
side2
  = quartet side1 side1 (rot t) t
corner1 = quartet nil nil nil u
corner2
  = quartet corner1 side1 (rot side1) u
pseudocorner
  = quartet
      corner2 side2 (rot side2) (rot t)
pseudolimit = cycle' pseudocorner
nonet
  p1_ayy
  p2_ayz
  p3_ayA
  p4_ayB
  p5_ayC
  p6_ayD
  p7_ayE
  p8_ayF
  p9_ayG
  = above
      1
      2
      (beside 1 2 p1_ayy (beside 1 1 p2_ayz p3_ayA))
      (above
         1
         1
         (beside 1 2 p4_ayB (beside 1 1 p5_ayC p6_ayD))
         (beside 1 2 p7_ayE (beside 1 1 p8_ayF p9_ayG)))
corner
  = nonet
      corner2
      side2
      side2
      (rot side2)
      u
      (rot t)
      (rot side2)
      (rot t)
      (rot q)
squarelimit = cycle' corner
fmt [] = "[]"
fmt (x_ayH : xs_ayI)
  = (showString "[\n" . showsPrec 0 x_ayH . showl_ayJ xs_ayI) ""
  where
      showl_ayJ [] s_ayK = showChar ']' s_ayK
      showl_ayJ (x_aH9 : xs_aHa) s_aHb
        = (showString ",\n" . showsPrec 0 x_aH9 . showl_ayJ xs_aHa) s_aHb
main
  = putStrLn (fmt (pseudolimit (0, 0) (640, 0) (0, 640)))

type Vec = (Int, Int)
type Line_segment = (Int, Int, Int, Int)
type Picture =
    Vec -> Vec -> Vec -> [Line_segment]

