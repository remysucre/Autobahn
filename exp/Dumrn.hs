module Main (main) where
start :: [[Int]]
start
  = [[], [], [], [], [], [], [], [], [], [], [], [], [], [],
     [0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1,
      1, 1, 1, 1, 0]]
gen n_ana board_anb
  = map row (shift (copy n_ana 0) board_anb)
row (last_anc, this_and, next_ane)
  = zipWith3
      elt
      (shift 0 last_anc)
      (shift 0 this_and)
      (shift 0 next_ane)
elt
  (a_aqQ, b_aqR, c_aqS)
  (d_aqT, e_aqU, f_aqV)
  (g_aqW, h_aqX, i_aqY)
  | tot_aqZ < 2 || tot_aqZ > 3 = 0
  | tot_aqZ == 3 = 1
  | otherwise = e_aqU
  where
      tot_aqZ
        = a_aqQ + b_aqR + c_aqS + d_aqT + f_aqV + g_aqW + h_aqX + i_aqY
shiftr x_aFq xs_aFr = [x_aFq] ++ init xs_aFr
shiftl x_aFs xs_aFt = tail xs_aFt ++ [x_aFs]
shift x_aFu xs_aFv
  = zip3 (shiftr x_aFu xs_aFv) xs_aFv (shiftl x_aFu xs_aFv)
copy 0 x_aFw = []
copy n_aFx x_aFy = x_aFy : copy (n_aFx - 1) x_aFy
disp (gen_aFz, xss_aFA)
  = gen_aFz
    ++
      "\n\
      \\n"
      ++
        (foldr (glue "\n") "" . map (concat . map star)) xss_aFA
star 0 = "  "
star 1 = " o"
glue s_aLx xs_aLy ys_aLz = xs_aLy ++ s_aLx ++ ys_aLz
limit (x_aLA : y_aLB : xs_aLC)
  | x_aLA == y_aLB = [x_aLA]
  | otherwise = x_aLA : limit (y_aLB : xs_aLC)
main
  = putStr (last generations_aLE)
  where
      sz_aLD = 30
      generations_aLE
        = (map disp
           . zip (map show [0 .. ]) . limit . iterate (gen sz_aLD))
            (take
               sz_aLD
               (map (take sz_aLD . (++ (copy sz_aLD 0))) start
                ++ copy sz_aLD (copy sz_aLD 0)))

