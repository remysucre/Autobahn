module Progfuns
       (tileprompt, tilequit, tiletrans, potatotile, State) where
import Mgrfuns
import Drawfuns
import Geomfuns
import Psfuns
import Interstate
import Auxprogfuns
import Layout
import Tilefuns
import Help
tileprompt :: a_aTc -> [Char]
tileprompt _ = ""
tilequit :: a_aTb -> [[Char]] -> Bool
tilequit _ (((!'q') : _) : _) = True
tilequit (!_) [] = True
tilequit _ (!_) = False
tiletrans :: Trans
tiletrans
  (!((!dlist_aTd), (!sel_aTe), (!tilist_aTf)))
  ((!((!((!'m') : ('s' : (!((!'a') : (!(' ' : (!rest_aTg)))))))) : inpt_aTh)))
  = if intsave x_aTi y_aTj then
        doo_aTk tsave
    else
        if intclear x_aTi y_aTj then
            doo_aTk tclear
        else
            if intget x_aTi y_aTj then
                doo_aTk tget
            else
                if intile4 x_aTi y_aTj then
                    doo_aTk t4'
                else
                    if inquit x_aTi y_aTj then
                        doo_aTk q
                    else
                        if inbigtile x_aTi y_aTj then
                            doo_aTk delsq
                        else
                            if intoalter x_aTi y_aTj then
                                doo_aTk tofiddle'
                            else
                                if intotile x_aTi y_aTj then
                                    doo_aTk totile'
                                else
                                    if intodraw x_aTi y_aTj then
                                        doo_aTk todesign'
                                    else
                                        if inpicarea x_aTi y_aTj then
                                            doo_aTk sel'
                                        else
                                            if inhelp x_aTi y_aTj then
                                                doo_aTk tohelp'
                                            else
                                                tiletrans
                                                  (dlist_aTd, sel_aTe, tilist_aTf) inpt_aTh
  where
      [x_aTi, (!y_aTj)] = stoil rest_aTg
      doo_aTk fun_aTl
        = fun_aTl rest_aTg (dlist_aTd, sel_aTe, tilist_aTf) inpt_aTh
tiletrans
  (!((!dlist_aTm), sel_aTn, tilist_aTo))
  (!(('m' : ('s' : (!('b' : (' ' : rest_aTp))))) : (!inpt_aTq)))
  = if inbigtile x_aTr y_aTs then
        inv' rest_aTp (dlist_aTm, sel_aTn, tilist_aTo) inpt_aTq
    else
        tiletrans
          (dlist_aTm, sel_aTn, tilist_aTo)
          (('m' : 's' : 'a' : ' ' : rest_aTp) : inpt_aTq)
  where
      [(!x_aTr), (!y_aTs)] = stoil rest_aTp
tiletrans
  (dlist_aTt, (!sel_aTu), tilist_aTv)
  ((!(('m' : ((!'s') : ('c' : (!(' ' : (!rest_aTw)))))) : (!inpt_aTx))))
  = if indesign x_aTy y_aTz then
        doo_aTA rl
    else
        if indsave x_aTy y_aTz then
            doo_aTA dsave
        else
            if indclear x_aTy y_aTz then
                doo_aTA dclear
            else
                if indget x_aTy y_aTz then
                    doo_aTA dget
                else
                    tiletrans
                      (dlist_aTt, sel_aTu, tilist_aTv)
                      (('m' : 's' : 'a' : ' ' : rest_aTw) : inpt_aTx)
  where
      (![x_aTy, y_aTz]) = stoil rest_aTw
      doo_aTA (!fun_aTB)
        = fun_aTB rest_aTw (dlist_aTt, sel_aTu, tilist_aTv) inpt_aTx
tiletrans
  state_aTC
  (((!('m' : ((!'s') : ((!'d') : ((!' ') : (!rest_aTD))))))) : (!inpt_aTE))
  = (inithelp ++ out_aTI, state_aTC, inpt_aTE)
  where
      (![x_aTF, y_aTG]) = stoil rest_aTD
      cf_aTH (!str_aTJ) = clear ++ str_aTJ
      out_aTI
        = if intodraw x_aTF y_aTG then
              cf_aTH helpdraw
          else
              if intotile x_aTF y_aTG then
                  cf_aTH helptile
              else
                  if intoalter x_aTF y_aTG then
                      cf_aTH helpalter
                  else
                      if intsave x_aTF y_aTG then
                          cf_aTH helptsave
                      else
                          if intclear x_aTF y_aTG then
                              cf_aTH helptclear
                          else
                              if intget x_aTF y_aTG then
                                  cf_aTH helptget
                              else
                                  if intile4 x_aTF y_aTG then
                                      cf_aTH helpt4
                                  else
                                      if inquit x_aTF y_aTG then
                                          cf_aTH helpquit
                                      else
                                          if inbigtile x_aTF y_aTG then
                                              cf_aTH helpbt
                                          else
                                              if inpicarea x_aTF y_aTG then
                                                  cf_aTH helppic
                                              else
                                                  if indesign x_aTF y_aTG then
                                                      cf_aTH helpdesign
                                                  else
                                                      if indsave x_aTF y_aTG then
                                                          cf_aTH helpdsave
                                                      else
                                                          if indclear x_aTF y_aTG then
                                                              cf_aTH helpdclear
                                                          else
                                                              if indget x_aTF y_aTG then
                                                                  cf_aTH helpdget
                                                              else
                                                                  if inhelp x_aTF y_aTG then
                                                                      cf_aTH inithelp
                                                                  else
                                                                      cf_aTH errmes
tiletrans
  ((!dlist_aTK), (!sel_aTL), tilist_aTM)
  (('c' : (!((!'s') : (!(' ' : (!rest_aTN)))))) : inpt_aTO)
  = if indgrid nstoilrest_aTQ then
        (linecircs_aTU ++ wnstoilrest_aTR, 
         (newele_aTT : dlist_aTK, sel_aTL, tilist_aTM), inpt_aTO)
    else
        ("", (dlist_aTK, sel_aTL, tilist_aTM), inpt_aTO)
  where
      nearline_aTP (![(!x0_aTV), y0_aTW, (!x1_aTX), y1_aTY])
        = [nearx x0_aTV, neary y0_aTW, nearx x1_aTX, neary y1_aTY]
      (!nstoilrest_aTQ) = nearline_aTP (stoil rest_aTN)
      (!wnstoilrest_aTR) = wline nstoilrest_aTQ
      (!cssr_aTS) = cs nstoilrest_aTQ
      newele_aTT = (nstoilrest_aTQ, snd cssr_aTS)
      (!linecircs_aTU) = fst cssr_aTS
tiletrans
  (!((!dlist_aU5), sel_aU6, (!tilist_aU7)))
  (!((!((!((!('r' : ('o' : ('t' : (' ' : rest_aU8))))))) : inpt_aU9))))
  = if lsrest_aUe == [0, 0] then
        ("", (dlist_aU5, sel_aU6, tilist_aU7), inpt_aU9)
    else
        (undo (put lsrest_aUe (orient xymax oldas_aUc wcoords_aUb))
         ++ put lsrest_aUe (orient xymax (rot oldas_aUc) wcoords_aUb), 
         (dlist_aU5, sel_aU6, newtilist_aUd), inpt_aU9)
  where
      stoilrest_aUa = stoil rest_aU8
      wcoords_aUb = map (map wscale) (map fst dlist_aU5)
      (!oldas_aUc) = assoc (sqid stoilrest_aUa) tilist_aU7
      newtilist_aUd
        = newas (sqid stoilrest_aUa) (rot oldas_aUc) tilist_aU7
      lsrest_aUe = btlocate stoilrest_aUa
tiletrans
  ((!dlist_aUf), (!sel_aUg), (!tilist_aUh))
  ((!(((!((!'p') : (!((!'u') : ((!'t') : ((!' ') : rest_aUi))))))) : (!inpt_aUj))))
  = if lsrest_aUm == [0, 0] then
        ("", (dlist_aUf, sel_aUg, tilist_aUh), inpt_aUj)
    else
        (undo (put lsrest_aUm (orient xymax oldas_aUo wcoords_aUp))
         ++ put lsrest_aUm (orient xymax sel_aUg wcoords_aUp), 
         (dlist_aUf, sel_aUg, newtilist_aUl), inpt_aUj)
  where
      stoilrest_aUk = stoil rest_aUi
      newtilist_aUl = newas (sqid stoilrest_aUk) sel_aUg tilist_aUh
      (!lsrest_aUm) = btlocate stoilrest_aUk
      coords_aUn = map fst dlist_aUf
      (!oldas_aUo) = assoc (sqid stoilrest_aUk) tilist_aUh
      wcoords_aUp = map (map wscale) coords_aUn
tiletrans state_aUq (!((!("" : inpt_aUr))))
  = (helpend ++ todesign, state_aUq, inpt_aUr)
tiletrans (!state_aUs) (_ : inpt_aUt)
  = ("", state_aUs, inpt_aUt)
todesign', totile', tofiddle', tohelp' ::
  [Char] -> Trans
todesign'
  (!_)
  (!(dlist_aUu, sel_aUv, (!tilist_aUw)))
  inpt_aUx
  = (cleara picarea
     ++
       picgrid
       ++
         cleara tilearea
         ++ tpgrid ++ showoris (map fst dlist_aUu) 1 ++ todesign, 
     (dlist_aUu, sel_aUv, tilist_aUw), inpt_aUx)
totile'
  _
  ((!dlist_aUy), sel_aUz, (!tilist_aUA))
  (!inpt_aUB)
  = (concat (map (showoris coords_aUC) [1 .. 8]) ++ totile, 
     (dlist_aUy, sel_aUz, tilist_aUA), inpt_aUB)
  where
      coords_aUC = map fst dlist_aUy
tofiddle'
  (!_)
  ((!dlist_a10K), sel_a10L, (!tilist_a10M))
  (!inpt_a10N)
  = (tofiddle, (dlist_a10K, sel_a10L, tilist_a10M), inpt_a10N)
tohelp'
  (!_)
  (!((!dlist_a10O), sel_a10P, (!tilist_a10Q)))
  (!inpt_a10R)
  = (tohelp, (dlist_a10O, sel_a10P, tilist_a10Q), inpt_a10R)
rl, dsave, dclear, dget ::
  [Char] -> Trans
rl
  rest_a10S
  (!(dlist_a10T, (!sel_a10U), (!tilist_a10V)))
  (!inpt_a10W)
  = (out_a10X, (newdlist_a10Y, sel_a10U, tilist_a10V), inpt_a10W)
  where
      (!(out_a10X, (!newdlist_a10Y)))
        = deline dlist_a10T (stoil rest_a10S)
dsave (!_) state_a10Z (!inpt_a110)
  = ("", state_a10Z, inpt_a110)
dclear
  rest_a111
  (dlist_a112, sel_a113, (!tilist_a114))
  (!inpt_a115)
  = (menumark "dclear"
     ++ newdraw ++ unmark sel_a113 ++ unmenumark "dclear", 
     ([], 1, initalist), inpt_a115)
dget (!_) (!state_a116) inpt_a117
  = ("", state_a116, inpt_a117)
sel', delsq, inv' ::
  [Char] -> Trans
sel'
  (!rest_a118)
  (!((!dlist_a119), sel_a11a, slist_a11b))
  (!inpt_a11c)
  = (unmark sel_a11a ++ mark newsel_a11e, 
     (dlist_a119, newsel_a11e, slist_a11b), inpt_a11c)
  where
      new_a11d = inbox (stoil rest_a118)
      (!newsel_a11e) = if new_a11d == 0 then sel_a11a else new_a11d
delsq
  rest_a11f
  (!((!dlist_a11g), sel_a11h, tilist_a11i))
  inpt_a11j
  = (undo (put lsrest_a11n (orient xymax oldas_a11m wcoords_a11k)), 
     (dlist_a11g, sel_a11h, newtilist_a11o), inpt_a11j)
  where
      (!wcoords_a11k) = map (map wscale) (map fst dlist_a11g)
      stoilrest_a11l = stoil rest_a11f
      oldas_a11m = assoc (sqid stoilrest_a11l) tilist_a11i
      lsrest_a11n = btlocate stoilrest_a11l
      (!newtilist_a11o) = newas (sqid stoilrest_a11l) 0 tilist_a11i
inv'
  rest_a11p
  (!(dlist_a11q, sel_a11r, (!tilist_a11s)))
  (!inpt_a11t)
  = if lsrest_a11y == [0, 0] then
        ("", (dlist_a11q, sel_a11r, tilist_a11s), inpt_a11t)
    else
        (undo (put lsrest_a11y (orient xymax oldas_a11w wcoords_a11v))
         ++ put lsrest_a11y (orient xymax (inv oldas_a11w) wcoords_a11v), 
         (dlist_a11q, sel_a11r, newtilist_a11x), inpt_a11t)
  where
      (!stoilrest_a11u) = stoil rest_a11p
      (!wcoords_a11v) = map (map wscale) (map fst dlist_a11q)
      oldas_a11w = assoc (sqid stoilrest_a11u) tilist_a11s
      newtilist_a11x
        = newas (sqid stoilrest_a11u) (inv oldas_a11w) tilist_a11s
      lsrest_a11y = btlocate stoilrest_a11u
tclear, tsave, tget, t4' ::
  [Char] -> Trans
tclear
  (!_)
  (dlist_a11z, sel_a11A, (!tilist_a11B))
  inpt_a11C
  = (menumark "tclear"
     ++ cleara tilearea ++ tpgrid ++ totile ++ unmenumark "tclear", 
     (dlist_a11z, sel_a11A, initalist), inpt_a11C)
tsave _ (!state_a11D) (!inpt_a11E)
  = ("", state_a11D, inpt_a11E)
tget _ state_a11F (!inpt_a11G)
  = ("", state_a11F, inpt_a11G)
t4'
  (!_)
  (!((!dlist_a11H), sel_a11I, (!tilist_a11J)))
  (!inpt_a11K)
  = (out_a11P, (dlist_a11H, sel_a11I, newtilist_a11O), inpt_a11K)
  where
      (!orilist_a11L)
        = pam assoc [(0, 0), (0, 1), (1, 0), (1, 1)] tilist_a11J
      (!wcoords_a11M) = map (map wscale) (map fst dlist_a11H)
      pic_a11N = t4 (pam (orient xymax) orilist_a11L wcoords_a11M)
      newtilist_a11O
        = zip alistind (concrep 4 (cr12_a11Q ++ cr34_a11R))
        where
            cr12_a11Q = concrep 4 [n1_a11S, n2_a11T]
            cr34_a11R = concrep 4 [n3_a11U, n4_a11V]
            (![n1_a11S, (!n2_a11T), n3_a11U, n4_a11V]) = orilist_a11L
      (!out_a11P)
        = menumark "t4"
          ++
            cleara tilearea
            ++ tile tpxorig tpyorig 4 4 pic_a11N ++ unmenumark "t4"
assoc :: (Eq a_aFH) => a_aFH -> [(a_aFH, b_aFI)] -> b_aFI
assoc (!i_a15j) (!((!(j_a15k, v_a15l)) : (!ivs_a15m)))
  = if i_a15j == j_a15k then
        v_a15l
    else
        assoc i_a15j ivs_a15m
q :: [Char] -> Trans
q (!_) state_a15n (!_) = ("", state_a15n, [])
newdraw :: [Char]
newdraw
  = cleara designarea
    ++
      dpgrid
      ++
        cleara picarea
        ++
          picgrid ++ cleara tilearea ++ tpgrid ++ invisibletext ++ todesign
potatotile :: State -> [[Char]] -> [Char]
potatotile
  = inter tileprompt tilequit tiletrans
stoil :: [Char] -> [Int]
stoil = map read . words

type State = ([([Int], [Int])], Int, [((Int, Int), Int)])
type Trans =
    State -> [[Char]] -> ([Char], State, [[Char]])

