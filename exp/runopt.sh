rm temp &&
# echo 1 && stack exec exp ~/nofib/shootout/n-body/Main.hs.opt >> temp &&
# echo 2 && stack exec exp ~/nofib/shootout/fasta/Main.hs.opt >> temp &&
echo 3 && stack exec exp ~/nofib/shootout/binary-trees/Main.hs.opt >> temp &&
# echo 4 && stack exec exp ~/nofib/shootout/pidigits/Main.hs.opt >> temp &&
# echo 5 && stack exec exp ~/nofib/shootout/fannkuch-redux/Main.hs.opt >> temp &&
echo 6 && stack exec exp ~/nofib/real/reptile/Geomfuns.hs.opt >> temp &&
echo 7 && stack exec exp ~/nofib/real/reptile/Layout.hs.opt >> temp &&
echo 8 && stack exec exp ~/nofib/real/reptile/Main.hs.opt >> temp &&
echo 9 && stack exec exp ~/nofib/real/reptile/Drawfuns.hs.opt >> temp &&
echo 10 && stack exec exp ~/nofib/real/reptile/Rational.hs.opt >> temp &&
echo 11 && stack exec exp ~/nofib/real/reptile/Interstate.hs.opt >> temp &&
echo 12 && stack exec exp ~/nofib/real/reptile/Help.hs.opt >> temp &&
echo 13 && stack exec exp ~/nofib/real/reptile/Auxprogfuns.hs.opt >> temp &&
echo 14 && stack exec exp ~/nofib/real/reptile/Diff.hs.opt >> temp &&
echo 15 && stack exec exp ~/nofib/real/reptile/Progfuns.hs.opt >> temp &&
echo 16 && stack exec exp ~/nofib/real/reptile/Mgrfuns.hs.opt >> temp &&
echo 17 && stack exec exp ~/nofib/real/reptile/Psfuns.hs.opt >> temp &&
echo 18 && stack exec exp ~/nofib/real/reptile/Tilefuns.hs.opt >> temp &&
echo 19 && stack exec exp ~/nofib/real/fem/Main.hs.opt >> temp &&
echo 20 && stack exec exp ~/nofib/real/fem/VBmatrix.hs.opt >> temp &&
echo 21 && stack exec exp ~/nofib/real/fem/VBlldecomp.hs.opt >> temp &&
echo 22 && stack exec exp ~/nofib/real/fem/Matrix.hs.opt >> temp &&
echo 23 && stack exec exp ~/nofib/real/fem/DB_interface.hs.opt >> temp &&
echo 24 && stack exec exp ~/nofib/real/fem/Assemble_stiffness.hs.opt >> temp &&
echo 25 && stack exec exp ~/nofib/real/fem/PrintSource.hs.opt >> temp &&
echo 26 && stack exec exp ~/nofib/real/fem/Pre_assemble.hs.opt >> temp &&
echo 27 && stack exec exp ~/nofib/real/fem/Database.hs.opt >> temp &&
echo 28 && stack exec exp ~/nofib/real/fem/Elemforce.hs.opt >> temp &&
echo 29 && stack exec exp ~/nofib/real/fem/Printuvwforce.hs.opt >> temp &&
echo 30 && stack exec exp ~/nofib/real/fem/Vector.hs.opt >> temp &&
echo 31 && stack exec exp ~/nofib/real/fem/Assemble_loadvec.hs.opt >> temp &&
echo 32 && stack exec exp ~/nofib/real/fem/Basics.hs.opt >> temp &&
echo 33 && stack exec exp ~/nofib/real/fem/Displacement.hs.opt >> temp &&
echo 34 && stack exec exp ~/nofib/real/fem/Degrees.hs.opt >> temp &&
echo 35 && stack exec exp ~/nofib/real/fem/Elemstif.hs.opt >> temp &&
echo 36 && stack exec exp ~/nofib/real/hidden/Main.hs.opt >> temp &&
echo 37 && stack exec exp ~/nofib/real/hidden/EdgePlate.hs.opt >> temp &&
echo 38 && stack exec exp ~/nofib/real/hidden/Cross.hs.opt >> temp &&
echo 39 && stack exec exp ~/nofib/real/hidden/Postscript.hs.opt >> temp &&
echo 40 && stack exec exp ~/nofib/real/hidden/Geometric.hs.opt >> temp &&
echo 41 && stack exec exp ~/nofib/real/hidden/Memo.hs.opt >> temp &&
echo 42 && stack exec exp ~/nofib/real/hidden/Preds.hs.opt >> temp &&
echo 43 && stack exec exp ~/nofib/real/hidden/MyIO.hs.opt >> temp &&
echo 44 && stack exec exp ~/nofib/real/hidden/Comparing.hs.opt >> temp &&
echo 45 && stack exec exp ~/nofib/real/hidden/Matrices.hs.opt >> temp &&
echo 46 && stack exec exp ~/nofib/real/hidden/Numbers.hs.opt >> temp &&
echo 47 && stack exec exp ~/nofib/real/hidden/Vectors.hs.opt >> temp &&
echo 48 && stack exec exp ~/nofib/real/hidden/Hide.hs.opt >> temp &&
echo 49 && stack exec exp ~/nofib/real/hidden/Solve.hs.opt >> temp &&
echo 50 && stack exec exp ~/nofib/real/cacheprof/Main.hs.opt >> temp &&
# echo 51 && stack exec exp ~/nofib/real/cacheprof/Generics.hs.opt >> temp &&
echo 52 && stack exec exp ~/nofib/real/cacheprof/Arch_x86.hs.opt >> temp &&
echo 53 && stack exec exp ~/nofib/real/fulsom/Main.hs.opt >> temp &&
echo 54 && stack exec exp ~/nofib/real/fulsom/Matrix.hs.opt >> temp &&
echo 55 && stack exec exp ~/nofib/real/fulsom/Quad.hs.opt >> temp &&
echo 56 && stack exec exp ~/nofib/real/fulsom/Kolor.hs.opt >> temp &&
# echo 57 && stack exec exp ~/nofib/real/fulsom/Interval.hs.opt >> temp &&
echo 58 && stack exec exp ~/nofib/real/fulsom/Shapes.hs.opt >> temp &&
echo 59 && stack exec exp ~/nofib/real/fulsom/Bah.hs.opt >> temp &&
echo 60 && stack exec exp ~/nofib/real/fulsom/Vector.hs.opt >> temp &&
echo 61 && stack exec exp ~/nofib/real/fulsom/Oct.hs.opt >> temp &&
echo 62 && stack exec exp ~/nofib/real/fulsom/Patchlevel.hs.opt >> temp &&
echo 63 && stack exec exp ~/nofib/real/fulsom/Types.hs.opt >> temp &&
echo 64 && stack exec exp ~/nofib/real/fulsom/Raster.hs.opt >> temp &&
echo 65 && stack exec exp ~/nofib/real/fulsom/Csg.hs.opt >> temp &&
# echo 66 && stack exec exp ~/nofib/real/maillist/Main.hs.opt >> temp &&
# echo 67 && stack exec exp ~/nofib/real/prolog/PrologData.hs.opt >> temp &&
# echo 68 && stack exec exp ~/nofib/real/prolog/Main.hs.opt >> temp &&
echo 69 && stack exec exp ~/nofib/real/prolog/Parse.hs.opt >> temp &&
echo 70 && stack exec exp ~/nofib/real/prolog/Subst.hs.opt >> temp &&
echo 71 && stack exec exp ~/nofib/real/prolog/StackEngine.hs.opt >> temp &&
echo 72 && stack exec exp ~/nofib/real/prolog/Version.hs.opt >> temp &&
echo 73 && stack exec exp ~/nofib/real/prolog/Interact.hs.opt >> temp &&
echo 74 && stack exec exp ~/nofib/real/prolog/Engine.hs.opt >> temp &&
echo 75 && stack exec exp ~/nofib/real/prolog/PureEngine.hs.opt >> temp &&
echo 76 && stack exec exp ~/nofib/real/pic/ElecField.hs.opt >> temp &&
echo 77 && stack exec exp ~/nofib/real/pic/Main.hs.opt >> temp &&
echo 78 && stack exec exp ~/nofib/real/pic/Consts.hs.opt >> temp &&
echo 79 && stack exec exp ~/nofib/real/pic/Potential.hs.opt >> temp &&
echo 80 && stack exec exp ~/nofib/real/pic/PicType.hs.opt >> temp &&
echo 81 && stack exec exp ~/nofib/real/pic/Pic.hs.opt >> temp &&
echo 82 && stack exec exp ~/nofib/real/pic/Utils.hs.opt >> temp &&
echo 83 && stack exec exp ~/nofib/real/pic/ChargeDensity.hs.opt >> temp &&
echo 84 && stack exec exp ~/nofib/real/pic/PushParticle.hs.opt >> temp &&
echo 85 && stack exec exp ~/nofib/real/anna/Dependancy.hs.opt >> temp &&
echo 86 && stack exec exp ~/nofib/real/anna/BaseDefs.hs.opt >> temp &&
echo 87 && stack exec exp ~/nofib/real/anna/Inverse.hs.opt >> temp &&
echo 88 && stack exec exp ~/nofib/real/anna/Main.hs.opt >> temp &&
echo 89 && stack exec exp ~/nofib/real/anna/PrettyPrint.hs.opt >> temp &&
echo 90 && stack exec exp ~/nofib/real/anna/FrontierDATAFN2.hs.opt >> temp &&
echo 91 && stack exec exp ~/nofib/real/anna/BarakiConc3.hs.opt >> temp &&
echo 92 && stack exec exp ~/nofib/real/anna/Simplify.hs.opt >> temp &&
echo 93 && stack exec exp ~/nofib/real/anna/AbstractMisc.hs.opt >> temp &&
echo 94 && stack exec exp ~/nofib/real/anna/AbsConc3.hs.opt >> temp &&
echo 95 && stack exec exp ~/nofib/real/anna/SuccsAndPreds2.hs.opt >> temp &&
echo 96 && stack exec exp ~/nofib/real/anna/MakeDomains.hs.opt >> temp &&
echo 97 && stack exec exp ~/nofib/real/anna/ReadTable.hs.opt >> temp &&
echo 98 && stack exec exp ~/nofib/real/anna/TypeCheck5.hs.opt >> temp &&
echo 99 && stack exec exp ~/nofib/real/anna/FrontierMisc2.hs.opt >> temp &&
echo 100 && stack exec exp ~/nofib/real/anna/BarakiMeet.hs.opt >> temp &&
echo 101 && stack exec exp ~/nofib/real/anna/AbstractEval2.hs.opt >> temp &&
echo 102 && stack exec exp ~/nofib/real/anna/TExpr2DExpr.hs.opt >> temp &&
echo 103 && stack exec exp ~/nofib/real/anna/EtaAbstract.hs.opt >> temp &&
echo 104 && stack exec exp ~/nofib/real/anna/StrictAn6.hs.opt >> temp &&
echo 105 && stack exec exp ~/nofib/real/anna/Utils.hs.opt >> temp &&
echo 106 && stack exec exp ~/nofib/real/anna/Constructors.hs.opt >> temp &&
echo 107 && stack exec exp ~/nofib/real/anna/FrontierGENERIC2.hs.opt >> temp &&
echo 108 && stack exec exp ~/nofib/real/anna/PrintResults.hs.opt >> temp &&
echo 109 && stack exec exp ~/nofib/real/anna/MyUtils.hs.opt >> temp &&
echo 110 && stack exec exp ~/nofib/real/anna/LambdaLift5.hs.opt >> temp &&
echo 111 && stack exec exp ~/nofib/real/anna/DomainExpr.hs.opt >> temp &&
echo 112 && stack exec exp ~/nofib/real/anna/Monster.hs.opt >> temp &&
echo 113 && stack exec exp ~/nofib/real/anna/Apply.hs.opt >> temp &&
echo 114 && stack exec exp ~/nofib/real/anna/SmallerLattice.hs.opt >> temp &&
echo 115 && stack exec exp ~/nofib/real/anna/AbstractVals2.hs.opt >> temp &&
echo 116 && stack exec exp ~/nofib/real/anna/Parser2.hs.opt >> temp &&
echo 117 && stack exec exp ~/nofib/real/fluid/S_Array.hs.opt >> temp &&
echo 118 && stack exec exp ~/nofib/real/fluid/Main.hs.opt >> temp &&
echo 119 && stack exec exp ~/nofib/real/fluid/Chl_decomp.hs.opt >> temp &&
echo 120 && stack exec exp ~/nofib/real/fluid/Elefac.hs.opt >> temp &&
echo 121 && stack exec exp ~/nofib/real/fluid/Jcb_method.hs.opt >> temp &&
echo 122 && stack exec exp ~/nofib/real/fluid/C_matrix.hs.opt >> temp &&
echo 123 && stack exec exp ~/nofib/real/fluid/TG_iter.hs.opt >> temp &&
echo 124 && stack exec exp ~/nofib/real/fluid/Norm.hs.opt >> temp &&
echo 125 && stack exec exp ~/nofib/real/fluid/Asb_routs.hs.opt >> temp &&
echo 126 && stack exec exp ~/nofib/real/fluid/Input_proc.hs.opt >> temp &&
echo 127 && stack exec exp ~/nofib/real/fluid/Rhs_Asb_routs.hs.opt >> temp &&
echo 128 && stack exec exp ~/nofib/real/fluid/Defs.hs.opt >> temp &&
echo 129 && stack exec exp ~/nofib/real/fluid/Min_degree.hs.opt >> temp &&
echo 130 && stack exec exp ~/nofib/real/fluid/Chl_routs.hs.opt >> temp &&
echo 131 && stack exec exp ~/nofib/real/fluid/Chl_method.hs.opt >> temp &&
echo 132 && stack exec exp ~/nofib/real/fluid/L_matrix.hs.opt >> temp &&
echo 133 && stack exec exp ~/nofib/real/fluid/S_matrix.hs.opt >> temp &&
echo 134 && stack exec exp ~/nofib/real/fluid/Tol_cal.hs.opt >> temp &&
echo 135 && stack exec exp ~/nofib/real/gg/Main.hs.opt >> temp &&
echo 136 && stack exec exp ~/nofib/real/gg/PSlib.hs.opt >> temp &&
echo 137 && stack exec exp ~/nofib/real/gg/Graph.hs.opt >> temp &&
echo 138 && stack exec exp ~/nofib/real/gg/Pool.hs.opt >> temp &&
echo 139 && stack exec exp ~/nofib/real/gg/Activity.hs.opt >> temp &&
echo 140 && stack exec exp ~/nofib/real/gg/Parse.hs.opt >> temp &&
echo 141 && stack exec exp ~/nofib/real/gg/StdLib.hs.opt >> temp &&
echo 142 && stack exec exp ~/nofib/real/gg/GRIP.hs.opt >> temp &&
echo 143 && stack exec exp ~/nofib/real/gg/Spark.hs.opt >> temp &&
echo 144 && stack exec exp ~/nofib/real/scs/Main.hs.opt >> temp &&
echo 145 && stack exec exp ~/nofib/real/scs/LinearAlgebra.hs.opt >> temp &&
echo 146 && stack exec exp ~/nofib/real/scs/Simulate.hs.opt >> temp &&
echo 147 && stack exec exp ~/nofib/real/scs/RandomFix.hs.opt >> temp &&
echo 148 && stack exec exp ~/nofib/real/scs/Parse.hs.opt >> temp &&
echo 149 && stack exec exp ~/nofib/real/scs/ParseLib.hs.opt >> temp &&
echo 150 && stack exec exp ~/nofib/real/scs/Types.hs.opt >> temp &&
echo 151 && stack exec exp ~/nofib/real/gamteb/Main.hs.opt >> temp &&
echo 152 && stack exec exp ~/nofib/real/gamteb/Consts.hs.opt >> temp &&
echo 153 && stack exec exp ~/nofib/real/gamteb/PhotoElec.hs.opt >> temp &&
echo 154 && stack exec exp ~/nofib/real/gamteb/GamtebMain.hs.opt >> temp &&
echo 155 && stack exec exp ~/nofib/real/gamteb/Distance.hs.opt >> temp &&
echo 156 && stack exec exp ~/nofib/real/gamteb/RoulSplit.hs.opt >> temp &&
echo 157 && stack exec exp ~/nofib/real/gamteb/TransPort.hs.opt >> temp &&
echo 158 && stack exec exp ~/nofib/real/gamteb/GamtebType.hs.opt >> temp &&
echo 159 && stack exec exp ~/nofib/real/gamteb/Utils.hs.opt >> temp &&
echo 160 && stack exec exp ~/nofib/real/gamteb/Compton.hs.opt >> temp &&
echo 161 && stack exec exp ~/nofib/real/gamteb/Pair.hs.opt >> temp &&
echo 162 && stack exec exp ~/nofib/real/gamteb/Output.hs.opt >> temp &&
echo 163 && stack exec exp ~/nofib/real/gamteb/InitTable.hs.opt >> temp &&
echo 164 && stack exec exp ~/nofib/real/rsa/Main.hs.opt >> temp &&
echo 165 && stack exec exp ~/nofib/real/rsa/Rsa.hs.opt >> temp &&
echo 166 && stack exec exp ~/nofib/real/symalg/RealM.hs.opt >> temp &&
echo 167 && stack exec exp ~/nofib/real/symalg/Main.hs.opt >> temp &&
echo 168 && stack exec exp ~/nofib/real/symalg/BasicNumber.hs.opt >> temp &&
echo 169 && stack exec exp ~/nofib/real/symalg/BasicNumberApprox.hs.opt >> temp &&
echo 170 && stack exec exp ~/nofib/real/symalg/Ast.hs.opt >> temp &&
echo 171 && stack exec exp ~/nofib/real/symalg/Print.hs.opt >> temp &&
echo 172 && stack exec exp ~/nofib/real/symalg/Op.hs.opt >> temp &&
echo 173 && stack exec exp ~/nofib/real/symalg/Eval.hs.opt >> temp &&
echo 174 && stack exec exp ~/nofib/real/symalg/Env.hs.opt >> temp &&
echo 175 && stack exec exp ~/nofib/real/symalg/Parser.hs.opt >> temp &&
# echo 176 && stack exec exp ~/nofib/real/symalg/Lexer.hs.opt >> temp &&
# echo 177 && stack exec exp ~/nofib/real/infer/Environment.hs.opt >> temp &&
# echo 178 && stack exec exp ~/nofib/real/infer/Substitution.hs.opt >> temp &&
# echo 179 && stack exec exp ~/nofib/real/infer/Main.hs.opt >> temp &&
# echo 180 && stack exec exp ~/nofib/real/infer/Infer.hs.opt >> temp &&
# echo 181 && stack exec exp ~/nofib/real/infer/Term.hs.opt >> temp &&
echo 182 && stack exec exp ~/nofib/real/infer/MyList.hs.opt >> temp &&
echo 183 && stack exec exp ~/nofib/real/infer/MaybeM.hs.opt >> temp &&
# echo 184 && stack exec exp ~/nofib/real/infer/Parse.hs.opt >> temp &&
# echo 185 && stack exec exp ~/nofib/real/infer/Type.hs.opt >> temp &&
echo 186 && stack exec exp ~/nofib/real/infer/FiniteMap.hs.opt >> temp &&
echo 187 && stack exec exp ~/nofib/real/infer/TestTerm.hs.opt >> temp &&
echo 188 && stack exec exp ~/nofib/real/infer/State.hs.opt >> temp &&
echo 189 && stack exec exp ~/nofib/real/infer/InferMonad.hs.opt >> temp &&
echo 190 && stack exec exp ~/nofib/real/infer/Shows.hs.opt >> temp &&
echo 191 && stack exec exp ~/nofib/real/infer/TestType.hs.opt >> temp &&
echo 192 && stack exec exp ~/nofib/real/infer/StateX.hs.opt >> temp &&
echo 193 && stack exec exp ~/nofib/spectral/scc/Main.hs.opt >> temp &&
echo 194 && stack exec exp ~/nofib/spectral/scc/Digraph.hs.opt >> temp &&
echo 195 && stack exec exp ~/nofib/spectral/minimax/Tree.hs.opt >> temp &&
echo 196 && stack exec exp ~/nofib/spectral/minimax/Main.hs.opt >> temp &&
echo 197 && stack exec exp ~/nofib/spectral/minimax/Wins.hs.opt >> temp &&
echo 198 && stack exec exp ~/nofib/spectral/minimax/Prog.hs.opt >> temp &&
echo 199 && stack exec exp ~/nofib/spectral/minimax/Game.hs.opt >> temp &&
echo 200 && stack exec exp ~/nofib/spectral/minimax/Board.hs.opt >> temp &&
echo 201 && stack exec exp ~/nofib/spectral/boyer2/Rewritefns.hs.opt >> temp &&
echo 202 && stack exec exp ~/nofib/spectral/boyer2/Main.hs.opt >> temp &&
echo 203 && stack exec exp ~/nofib/spectral/boyer2/Rulebasetext.hs.opt >> temp &&
# echo 204 && stack exec exp ~/nofib/spectral/boyer2/Lisplikefns.hs.opt >> temp &&
# echo 205 && stack exec exp ~/nofib/spectral/boyer2/Checker.hs.opt >> temp &&
echo 206 && stack exec exp ~/nofib/spectral/integer/Main.hs.opt >> temp &&
echo 207 && stack exec exp ~/nofib/spectral/cryptarithm1/Main.hs.opt >> temp &&
echo 208 && stack exec exp ~/nofib/spectral/sorting/Main.hs.opt >> temp &&
echo 209 && stack exec exp ~/nofib/spectral/sorting/Sort.hs.opt >> temp &&
echo 210 && stack exec exp ~/nofib/spectral/expert/Match.hs.opt >> temp &&
echo 211 && stack exec exp ~/nofib/spectral/expert/Knowledge.hs.opt >> temp &&
echo 212 && stack exec exp ~/nofib/spectral/expert/Main.hs.opt >> temp &&
echo 213 && stack exec exp ~/nofib/spectral/expert/Result.hs.opt >> temp &&
echo 214 && stack exec exp ~/nofib/spectral/expert/Table.hs.opt >> temp &&
echo 215 && stack exec exp ~/nofib/spectral/expert/Search.hs.opt >> temp &&
# echo 216 && stack exec exp ~/nofib/spectral/simple/Main.hs.opt >> temp &&
# echo 217 && stack exec exp ~/nofib/spectral/cichelli/Main.hs.opt >> temp &&
echo 218 && stack exec exp ~/nofib/spectral/cichelli/Interval.hs.opt >> temp &&
echo 219 && stack exec exp ~/nofib/spectral/cichelli/Auxil.hs.opt >> temp &&
echo 220 && stack exec exp ~/nofib/spectral/cichelli/Prog.hs.opt >> temp &&
echo 221 && stack exec exp ~/nofib/spectral/awards/Main.hs.opt >> temp &&
echo 222 && stack exec exp ~/nofib/spectral/awards/QSort.hs.opt >> temp &&
echo 223 && stack exec exp ~/nofib/spectral/power/Main.hs.opt >> temp &&
echo 224 && stack exec exp ~/nofib/spectral/multiplier/Main.hs.opt >> temp &&
# echo 225 && stack exec exp ~/nofib/spectral/puzzle/Main.hs.opt >> temp &&
# echo 226 && stack exec exp ~/nofib/spectral/cse/Main.hs.opt >> temp &&
echo 227 && stack exec exp ~/nofib/spectral/cse/StateMonad.hs.opt >> temp &&
echo 228 && stack exec exp ~/nofib/spectral/eliza/Main.hs.opt >> temp &&
echo 229 && stack exec exp ~/nofib/spectral/clausify/Main.hs.opt >> temp &&
# echo 230 && stack exec exp ~/nofib/spectral/ansi/Main.hs.opt >> temp &&
echo 231 && stack exec exp ~/nofib/spectral/atom/Main.hs.opt >> temp &&
echo 232 && stack exec exp ~/nofib/spectral/lcss/Main.hs.opt >> temp &&
# echo 233 && stack exec exp ~/nofib/spectral/treejoin/Main.hs.opt >> temp &&
echo 234 && stack exec exp ~/nofib/spectral/gcd/Main.hs.opt >> temp &&
echo 235 && stack exec exp ~/nofib/spectral/calendar/Main.hs.opt >> temp &&
# echo 236 && stack exec exp ~/nofib/spectral/cryptarithm2/Main.hs.opt >> temp &&
echo 237 && stack exec exp ~/nofib/spectral/fish/Main.hs.opt >> temp &&
echo 238 && stack exec exp ~/nofib/spectral/life/Main.hs.opt >> temp &&
echo 239 && stack exec exp ~/nofib/spectral/banner/Main.hs.opt >> temp &&
echo 240 && stack exec exp ~/nofib/spectral/pretty/Main.hs.opt >> temp &&
echo 241 && stack exec exp ~/nofib/spectral/pretty/Pretty.hs.opt >> temp &&
echo 242 && stack exec exp ~/nofib/spectral/pretty/CharSeq.hs.opt >> temp &&
echo 243 && stack exec exp ~/nofib/imaginary/bernouilli/Main.hs.opt >> temp &&
echo 244 && stack exec exp ~/nofib/imaginary/gen_regexps/Main.hs.opt >> temp &&
echo 245 && stack exec exp ~/nofib/imaginary/x2n1/Main.hs.opt >> temp &&
echo 246 && stack exec exp ~/nofib/imaginary/wheel-sieve2/Main.hs.opt >> temp &&
echo 247 && stack exec exp ~/nofib/imaginary/primes/Main.hs.opt >> temp &&
echo 248 && stack exec exp ~/nofib/imaginary/exp3_8/Main.hs.opt >> temp &&
# echo 249 && stack exec exp ~/nofib/imaginary/kahan/Main.hs.opt >> temp &&
echo 250 && stack exec exp ~/nofib/imaginary/queens/Main.hs.opt >> temp &&
echo 251 && stack exec exp ~/nofib/imaginary/paraffins/Main.hs.opt >> temp &&
echo 252 && stack exec exp ~/nofib/imaginary/wheel-sieve1/Main.hs.opt >> temp &&
echo 253 && stack exec exp ~/nofib/imaginary/tak/Main.hs.opt >> temp &&
echo 254 && stack exec exp ~/nofib/imaginary/rfib/Main.hs.opt >> temp &&
echo 255 && stack exec exp ~/nofib/imaginary/integrate/Main.hs.opt >> temp 
