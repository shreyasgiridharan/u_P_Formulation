 GiD Post Results File 1.0
 GaussPoints "Material_Point" Elemtype Quadrilateral
 Number of Gauss Points: 4
 Natural Coordinates: Internal
 end gausspoints
 Result "Boundary" "MPM"           1 Vector OnNodes
 ComponentNames "X-fix", "Y-fix"
 values
      1      1      1
      2      1      1
      3      1      0
      4      1      0
 end values
 Result "displacement" "MPM"           1 Vector OnNodes
 ComponentNames "comp. x", "comp. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000   0.135276E-024
      4   0.000000E+000   0.135276E-024
 end values
 Result "velocity" "MPM"           1 Vector OnNodes
 ComponentNames "vel. x", "vel. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000   0.135276E-019
      4   0.000000E+000   0.135276E-019
 end values
 Result "Internal_force" "MPM"           1 Vector OnNodes
 ComponentNames "Inrf. x", "Inrf. y"
 values
      1   0.390000E+001   0.530000E+001
      2  -0.390000E+001   0.530000E+001
      3   0.390000E+001  -0.530000E+001
      4  -0.390000E+001  -0.530000E+001
 end values
 Result "Gravity_force" "MPM"           1 Vector OnNodes
 ComponentNames "Grvf. x", "Grvf. y"
 values
      1   0.000000E+000  -0.530000E+001
      2   0.000000E+000  -0.530000E+001
      3   0.000000E+000  -0.530000E+001
      4   0.000000E+000  -0.530000E+001
 end values
 Result "Mass" "MPM"           1 Vector OnNodes
 ComponentNames "Mass. x", "Mass. y"
 values
      1   0.650000E+000   0.650000E+000
      2   0.650000E+000   0.650000E+000
      3   0.650000E+000   0.650000E+000
      4   0.650000E+000   0.650000E+000
 end values
 Result "Stress" "MPM"           1 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigma xx", "sigma yy", "sigma zz", "sigma xy"
 values
      1 -0.779778E+0001 -0.105978E+0002 -0.779778E+0001  0.000000E+0000
 -0.779852E+0001 -0.105985E+0002 -0.779778E+0001  0.000000E+0000
 -0.779778E+0001 -0.105978E+0002 -0.779778E+0001  0.000000E+0000
 -0.779704E+0001 -0.105970E+0002 -0.779778E+0001  0.000000E+0000
 end values
 Result "StressE" "MPM"           1 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigmaE xx", "sigmaE yy", "sigmaE zz", "sigmaE xy"
 values
      1 -0.280000E+0001 -0.560000E+0001 -0.280000E+0001  0.000000E+0000
 -0.280000E+0001 -0.560000E+0001 -0.280000E+0001  0.000000E+0000
 -0.280000E+0001 -0.560000E+0001 -0.280000E+0001  0.000000E+0000
 -0.280000E+0001 -0.560000E+0001 -0.280000E+0001  0.000000E+0000
 end values
 Result "Pressure" "MPM"           1 Vector OnGaussPoints "Material_Point"
 ComponentNames "Pressure xx", "Pressure yy", "Pressure zz", "Pressure xy"
 values
      1 -0.499778E+0001 -0.499778E+0001 -0.499778E+0001  0.000000E+0000
 -0.499852E+0001 -0.499852E+0001 -0.499778E+0001  0.000000E+0000
 -0.499778E+0001 -0.499778E+0001 -0.499778E+0001  0.000000E+0000
 -0.499704E+0001 -0.499704E+0001 -0.499778E+0001  0.000000E+0000
 end values
 Result "Strain" "MPM"           1 Vector OnGaussPoints "Material_Point"
 ComponentNames "eps xx", "eps yy", "eps xy"
 values
      1   0.000000E+000   0.135276E-024   0.000000E+000
   0.000000E+000   0.135276E-024   0.000000E+000
   0.000000E+000   0.135276E-024   0.000000E+000
   0.000000E+000   0.135276E-024   0.000000E+000
 end values
 Result "Plastic strain" "MPM"           1 Vector OnGaussPoints "Material_Point"
 ComponentNames "EpsP xx", "EpsP yy", "EpsP zz", "EpsP xy"
 values
      1 -0.779778E+0001 -0.105978E+0002  0.000000E+0000 -0.779778E+0001
 -0.779852E+0001 -0.105985E+0002  0.000000E+0000 -0.779778E+0001
 -0.779778E+0001 -0.105978E+0002  0.000000E+0000 -0.779778E+0001
 -0.779704E+0001 -0.105970E+0002  0.000000E+0000 -0.779778E+0001
 end values
 Result "Failure" "MPM"           1 Scalar OnGaussPoints "Material_Point"
 ComponentNames "Failure Indicator"
 values
      1  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
 end values
 Result "displacement" "MPM"           2 Vector OnNodes
 ComponentNames "comp. x", "comp. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000  -0.103079E-003
      4   0.000000E+000  -0.720542E-004
 end values
 Result "velocity" "MPM"           2 Vector OnNodes
 ComponentNames "vel. x", "vel. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000  -0.721679E-003
      4   0.000000E+000  -0.440272E-003
 end values
 Result "Internal_force" "MPM"           2 Vector OnNodes
 ComponentNames "Inrf. x", "Inrf. y"
 values
      1   0.351625E+001   0.471433E+001
      2  -0.363261E+001   0.513042E+001
      3   0.287164E+001  -0.483069E+001
      4  -0.275527E+001  -0.501406E+001
 end values
 Result "Gravity_force" "MPM"           2 Vector OnNodes
 ComponentNames "Grvf. x", "Grvf. y"
 values
      1   0.000000E+000  -0.530000E+001
      2   0.000000E+000  -0.530000E+001
      3   0.000000E+000  -0.530000E+001
      4   0.000000E+000  -0.530000E+001
 end values
 Result "Mass" "MPM"           2 Vector OnNodes
 ComponentNames "Mass. x", "Mass. y"
 values
      1   0.650000E+000   0.650000E+000
      2   0.650000E+000   0.650000E+000
      3   0.650000E+000   0.650000E+000
      4   0.650000E+000   0.650000E+000
 end values
 Result "Stress" "MPM"           2 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigma xx", "sigma yy", "sigma zz", "sigma xy"
 values
      1 -0.705252E+0001 -0.105766E+0002 -0.705252E+0001  0.491847E-0001
 -0.835954E+0001 -0.117493E+0002 -0.705252E+0001  0.491847E-0001
 -0.572332E+0001 -0.911304E+0001 -0.705252E+0001  0.183560E+0000
 -0.441629E+0001 -0.794039E+0001 -0.705252E+0001  0.183560E+0000
 end values
 Result "StressE" "MPM"           2 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigmaE xx", "sigmaE yy", "sigmaE zz", "sigmaE xy"
 values
      1 -0.352193E+0001 -0.704603E+0001 -0.352193E+0001  0.491847E-0001
 -0.338796E+0001 -0.677769E+0001 -0.352193E+0001  0.491847E-0001
 -0.338796E+0001 -0.677769E+0001 -0.352193E+0001  0.183560E+0000
 -0.352193E+0001 -0.704603E+0001 -0.352193E+0001  0.183560E+0000
 end values
 Result "Pressure" "MPM"           2 Vector OnGaussPoints "Material_Point"
 ComponentNames "Pressure xx", "Pressure yy", "Pressure zz", "Pressure xy"
 values
      1 -0.353058E+0001 -0.353058E+0001 -0.353058E+0001  0.000000E+0000
 -0.497158E+0001 -0.497158E+0001 -0.353058E+0001  0.000000E+0000
 -0.233535E+0001 -0.233535E+0001 -0.353058E+0001  0.000000E+0000
 -0.894359E+0000 -0.894359E+0000 -0.353058E+0001  0.000000E+0000
 end values
 Result "Strain" "MPM"           2 Vector OnGaussPoints "Material_Point"
 ComponentNames "eps xx", "eps yy", "eps xy"
 values
      1   0.000000E+000  -0.965227E-004   0.655632E-005
   0.000000E+000  -0.786105E-004   0.655632E-005
   0.000000E+000  -0.786105E-004   0.244685E-004
   0.000000E+000  -0.965227E-004   0.244685E-004
 end values
 Result "Plastic strain" "MPM"           2 Vector OnGaussPoints "Material_Point"
 ComponentNames "EpsP xx", "EpsP yy", "EpsP zz", "EpsP xy"
 values
      1 -0.705252E+0001 -0.105766E+0002  0.491847E-0001 -0.705252E+0001
 -0.835954E+0001 -0.117493E+0002  0.491847E-0001 -0.705252E+0001
 -0.572332E+0001 -0.911304E+0001  0.183560E+0000 -0.705252E+0001
 -0.441629E+0001 -0.794039E+0001  0.183560E+0000 -0.705252E+0001
 end values
 Result "Failure" "MPM"           2 Scalar OnGaussPoints "Material_Point"
 ComponentNames "Failure Indicator"
 values
      1  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
 end values
 Result "displacement" "MPM"           3 Vector OnNodes
 ComponentNames "comp. x", "comp. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000  -0.149352E-003
      4   0.000000E+000  -0.994210E-004
 end values
 Result "velocity" "MPM"           3 Vector OnNodes
 ComponentNames "vel. x", "vel. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000  -0.274545E-003
      4   0.000000E+000  -0.158913E-003
 end values
 Result "Internal_force" "MPM"           3 Vector OnNodes
 ComponentNames "Inrf. x", "Inrf. y"
 values
      1   0.370112E+001   0.493419E+001
      2  -0.388841E+001   0.538396E+001
      3   0.288391E+001  -0.512147E+001
      4  -0.269663E+001  -0.519668E+001
 end values
 Result "Gravity_force" "MPM"           3 Vector OnNodes
 ComponentNames "Grvf. x", "Grvf. y"
 values
      1   0.000000E+000  -0.530000E+001
      2   0.000000E+000  -0.530000E+001
      3   0.000000E+000  -0.530000E+001
      4   0.000000E+000  -0.530000E+001
 end values
 Result "Mass" "MPM"           3 Vector OnNodes
 ComponentNames "Mass. x", "Mass. y"
 values
      1   0.650000E+000   0.650000E+000
      2   0.650000E+000   0.650000E+000
      3   0.650000E+000   0.650000E+000
      4   0.650000E+000   0.650000E+000
 end values
 Result "Stress" "MPM"           3 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigma xx", "sigma yy", "sigma zz", "sigma xy"
 values
      1 -0.765400E+0001 -0.114953E+0002 -0.765400E+0001  0.791575E-0001
 -0.899581E+0001 -0.126208E+0002 -0.765400E+0001  0.791575E-0001
 -0.551609E+0001 -0.914109E+0001 -0.765400E+0001  0.295420E+0000
 -0.417427E+0001 -0.801554E+0001 -0.765400E+0001  0.295420E+0000
 end values
 Result "StressE" "MPM"           3 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigmaE xx", "sigmaE yy", "sigmaE zz", "sigmaE xy"
 values
      1 -0.383815E+0001 -0.767941E+0001 -0.383815E+0001  0.791575E-0001
 -0.362253E+0001 -0.724753E+0001 -0.383815E+0001  0.791575E-0001
 -0.362253E+0001 -0.724753E+0001 -0.383815E+0001  0.295420E+0000
 -0.383815E+0001 -0.767941E+0001 -0.383815E+0001  0.295420E+0000
 end values
 Result "Pressure" "MPM"           3 Vector OnGaussPoints "Material_Point"
 ComponentNames "Pressure xx", "Pressure yy", "Pressure zz", "Pressure xy"
 values
      1 -0.381585E+0001 -0.381585E+0001 -0.381585E+0001  0.000000E+0000
 -0.537328E+0001 -0.537328E+0001 -0.381585E+0001  0.000000E+0000
 -0.189356E+0001 -0.189356E+0001 -0.381585E+0001  0.000000E+0000
 -0.336125E+0000 -0.336125E+0000 -0.381585E+0001  0.000000E+0000
 end values
 Result "Strain" "MPM"           3 Vector OnGaussPoints "Material_Point"
 ComponentNames "eps xx", "eps yy", "eps xy"
 values
      1   0.000000E+000  -0.138800E-003   0.105517E-004
   0.000000E+000  -0.109973E-003   0.105517E-004
   0.000000E+000  -0.109973E-003   0.393795E-004
   0.000000E+000  -0.138800E-003   0.393795E-004
 end values
 Result "Plastic strain" "MPM"           3 Vector OnGaussPoints "Material_Point"
 ComponentNames "EpsP xx", "EpsP yy", "EpsP zz", "EpsP xy"
 values
      1 -0.765400E+0001 -0.114953E+0002  0.791575E-0001 -0.765400E+0001
 -0.899581E+0001 -0.126208E+0002  0.791575E-0001 -0.765400E+0001
 -0.551609E+0001 -0.914109E+0001  0.295420E+0000 -0.765400E+0001
 -0.417427E+0001 -0.801554E+0001  0.295420E+0000 -0.765400E+0001
 end values
 Result "Failure" "MPM"           3 Scalar OnGaussPoints "Material_Point"
 ComponentNames "Failure Indicator"
 values
      1  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
 end values
 Result "displacement" "MPM"           4 Vector OnNodes
 ComponentNames "comp. x", "comp. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000  -0.166944E-003
      4   0.000000E+000  -0.109568E-003
 end values
 Result "velocity" "MPM"           4 Vector OnNodes
 ComponentNames "vel. x", "vel. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000  -0.104327E-003
      4   0.000000E+000  -0.600398E-004
 end values
 Result "Internal_force" "MPM"           4 Vector OnNodes
 ComponentNames "Inrf. x", "Inrf. y"
 values
      1   0.376864E+001   0.501695E+001
      2  -0.398385E+001   0.547617E+001
      3   0.288730E+001  -0.523216E+001
      4  -0.267209E+001  -0.526096E+001
 end values
 Result "Gravity_force" "MPM"           4 Vector OnNodes
 ComponentNames "Grvf. x", "Grvf. y"
 values
      1   0.000000E+000  -0.530000E+001
      2   0.000000E+000  -0.530000E+001
      3   0.000000E+000  -0.530000E+001
      4   0.000000E+000  -0.530000E+001
 end values
 Result "Mass" "MPM"           4 Vector OnNodes
 ComponentNames "Mass. x", "Mass. y"
 values
      1   0.650000E+000   0.650000E+000
      2   0.650000E+000   0.650000E+000
      3   0.650000E+000   0.650000E+000
      4   0.650000E+000   0.650000E+000
 end values
 Result "Stress" "MPM"           4 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigma xx", "sigma yy", "sigma zz", "sigma xy"
 values
      1 -0.788410E+0001 -0.118455E+0002 -0.788410E+0001  0.909589E-0001
 -0.922638E+0001 -0.129393E+0002 -0.788410E+0001  0.909589E-0001
 -0.542779E+0001 -0.914072E+0001 -0.788410E+0001  0.339463E+0000
 -0.408552E+0001 -0.804695E+0001 -0.788410E+0001  0.339463E+0000
 end values
 Result "StressE" "MPM"           4 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigmaE xx", "sigmaE yy", "sigmaE zz", "sigmaE xy"
 values
      1 -0.395795E+0001 -0.791939E+0001 -0.395795E+0001  0.909589E-0001
 -0.371019E+0001 -0.742312E+0001 -0.395795E+0001  0.909589E-0001
 -0.371019E+0001 -0.742312E+0001 -0.395795E+0001  0.339463E+0000
 -0.395795E+0001 -0.791939E+0001 -0.395795E+0001  0.339463E+0000
 end values
 Result "Pressure" "MPM"           4 Vector OnGaussPoints "Material_Point"
 ComponentNames "Pressure xx", "Pressure yy", "Pressure zz", "Pressure xy"
 values
      1 -0.392615E+0001 -0.392615E+0001 -0.392615E+0001  0.000000E+0000
 -0.551618E+0001 -0.551618E+0001 -0.392615E+0001  0.000000E+0000
 -0.171760E+0001 -0.171760E+0001 -0.392615E+0001  0.000000E+0000
 -0.127562E+0000 -0.127562E+0000 -0.392615E+0001  0.000000E+0000
 end values
 Result "Strain" "MPM"           4 Vector OnGaussPoints "Material_Point"
 ComponentNames "eps xx", "eps yy", "eps xy"
 values
      1   0.000000E+000  -0.154819E-003   0.121248E-004
   0.000000E+000  -0.121693E-003   0.121248E-004
   0.000000E+000  -0.121693E-003   0.452505E-004
   0.000000E+000  -0.154819E-003   0.452505E-004
 end values
 Result "Plastic strain" "MPM"           4 Vector OnGaussPoints "Material_Point"
 ComponentNames "EpsP xx", "EpsP yy", "EpsP zz", "EpsP xy"
 values
      1 -0.788410E+0001 -0.118455E+0002  0.909589E-0001 -0.788410E+0001
 -0.922638E+0001 -0.129393E+0002  0.909589E-0001 -0.788410E+0001
 -0.542779E+0001 -0.914072E+0001  0.339463E+0000 -0.788410E+0001
 -0.408552E+0001 -0.804695E+0001  0.339463E+0000 -0.788410E+0001
 end values
 Result "Failure" "MPM"           4 Scalar OnGaussPoints "Material_Point"
 ComponentNames "Failure Indicator"
 values
      1  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
 end values
 Result "displacement" "MPM"           5 Vector OnNodes
 ComponentNames "comp. x", "comp. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000  -0.173628E-003
      4   0.000000E+000  -0.113414E-003
 end values
 Result "velocity" "MPM"           5 Vector OnNodes
 ComponentNames "vel. x", "vel. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000  -0.396397E-004
      4   0.000000E+000  -0.227984E-004
 end values
 Result "Internal_force" "MPM"           5 Vector OnNodes
 ComponentNames "Inrf. x", "Inrf. y"
 values
      1   0.379419E+001   0.504836E+001
      2  -0.402005E+001   0.551103E+001
      3   0.288854E+001  -0.527422E+001
      4  -0.266268E+001  -0.528517E+001
 end values
 Result "Gravity_force" "MPM"           5 Vector OnNodes
 ComponentNames "Grvf. x", "Grvf. y"
 values
      1   0.000000E+000  -0.530000E+001
      2   0.000000E+000  -0.530000E+001
      3   0.000000E+000  -0.530000E+001
      4   0.000000E+000  -0.530000E+001
 end values
 Result "Mass" "MPM"           5 Vector OnNodes
 ComponentNames "Mass. x", "Mass. y"
 values
      1   0.650000E+000   0.650000E+000
      2   0.650000E+000   0.650000E+000
      3   0.650000E+000   0.650000E+000
      4   0.650000E+000   0.650000E+000
 end values
 Result "Stress" "MPM"           5 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigma xx", "sigma yy", "sigma zz", "sigma xy"
 values
      1 -0.797159E+0001 -0.119787E+0002 -0.797159E+0001  0.954596E-0001
 -0.931353E+0001 -0.130598E+0002 -0.797159E+0001  0.954596E-0001
 -0.539386E+0001 -0.914014E+0001 -0.797159E+0001  0.356260E+0000
 -0.405192E+0001 -0.805900E+0001 -0.797159E+0001  0.356260E+0000
 end values
 Result "StressE" "MPM"           5 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigmaE xx", "sigmaE yy", "sigmaE zz", "sigmaE xy"
 values
      1 -0.400346E+0001 -0.801054E+0001 -0.400346E+0001  0.954596E-0001
 -0.374344E+0001 -0.748972E+0001 -0.400346E+0001  0.954596E-0001
 -0.374344E+0001 -0.748972E+0001 -0.400346E+0001  0.356260E+0000
 -0.400346E+0001 -0.801054E+0001 -0.400346E+0001  0.356260E+0000
 end values
 Result "Pressure" "MPM"           5 Vector OnGaussPoints "Material_Point"
 ComponentNames "Pressure xx", "Pressure yy", "Pressure zz", "Pressure xy"
 values
      1 -0.396813E+0001 -0.396813E+0001 -0.396813E+0001  0.000000E+0000
 -0.557008E+0001 -0.557008E+0001 -0.396813E+0001  0.000000E+0000
 -0.165042E+0001 -0.165042E+0001 -0.396813E+0001  0.000000E+0000
 -0.484613E-0001 -0.484613E-0001 -0.396813E+0001  0.000000E+0000
 end values
 Result "Strain" "MPM"           5 Vector OnGaussPoints "Material_Point"
 ComponentNames "eps xx", "eps yy", "eps xy"
 values
      1   0.000000E+000  -0.160903E-003   0.127248E-004
   0.000000E+000  -0.126139E-003   0.127248E-004
   0.000000E+000  -0.126139E-003   0.474895E-004
   0.000000E+000  -0.160903E-003   0.474895E-004
 end values
 Result "Plastic strain" "MPM"           5 Vector OnGaussPoints "Material_Point"
 ComponentNames "EpsP xx", "EpsP yy", "EpsP zz", "EpsP xy"
 values
      1 -0.797159E+0001 -0.119787E+0002  0.954596E-0001 -0.797159E+0001
 -0.931353E+0001 -0.130598E+0002  0.954596E-0001 -0.797159E+0001
 -0.539386E+0001 -0.914014E+0001  0.356260E+0000 -0.797159E+0001
 -0.405192E+0001 -0.805900E+0001  0.356260E+0000 -0.797159E+0001
 end values
 Result "Failure" "MPM"           5 Scalar OnGaussPoints "Material_Point"
 ComponentNames "Failure Indicator"
 values
      1  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
 end values
 Result "displacement" "MPM"           6 Vector OnNodes
 ComponentNames "comp. x", "comp. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000  -0.176168E-003
      4   0.000000E+000  -0.114875E-003
 end values
 Result "velocity" "MPM"           6 Vector OnNodes
 ComponentNames "vel. x", "vel. y"
 values
      1   0.000000E+000   0.000000E+000
      2   0.000000E+000   0.000000E+000
      3   0.000000E+000  -0.150612E-004
      4   0.000000E+000  -0.866172E-005
 end values
 Result "Internal_force" "MPM"           6 Vector OnNodes
 ComponentNames "Inrf. x", "Inrf. y"
 values
      1   0.380389E+001   0.506030E+001
      2  -0.403379E+001   0.552427E+001
      3   0.288901E+001  -0.529021E+001
      4  -0.265910E+001  -0.529437E+001
 end values
 Result "Gravity_force" "MPM"           6 Vector OnNodes
 ComponentNames "Grvf. x", "Grvf. y"
 values
      1   0.000000E+000  -0.530000E+001
      2   0.000000E+000  -0.530000E+001
      3   0.000000E+000  -0.530000E+001
      4   0.000000E+000  -0.530000E+001
 end values
 Result "Mass" "MPM"           6 Vector OnNodes
 ComponentNames "Mass. x", "Mass. y"
 values
      1   0.650000E+000   0.650000E+000
      2   0.650000E+000   0.650000E+000
      3   0.650000E+000   0.650000E+000
      4   0.650000E+000   0.650000E+000
 end values
 Result "Stress" "MPM"           6 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigma xx", "sigma yy", "sigma zz", "sigma xy"
 values
      1 -0.800484E+0001 -0.120293E+0002 -0.800484E+0001  0.971703E-0001
 -0.934662E+0001 -0.131056E+0002 -0.800484E+0001  0.971703E-0001
 -0.538095E+0001 -0.913990E+0001 -0.800484E+0001  0.362645E+0000
 -0.403917E+0001 -0.806358E+0001 -0.800484E+0001  0.362645E+0000
 end values
 Result "StressE" "MPM"           6 Vector OnGaussPoints "Material_Point"
 ComponentNames "sigmaE xx", "sigmaE yy", "sigmaE zz", "sigmaE xy"
 values
      1 -0.402075E+0001 -0.804517E+0001 -0.402075E+0001  0.971703E-0001
 -0.375607E+0001 -0.751502E+0001 -0.402075E+0001  0.971703E-0001
 -0.375607E+0001 -0.751502E+0001 -0.402075E+0001  0.362645E+0000
 -0.402075E+0001 -0.804517E+0001 -0.402075E+0001  0.362645E+0000
 end values
 Result "Pressure" "MPM"           6 Vector OnGaussPoints "Material_Point"
 ComponentNames "Pressure xx", "Pressure yy", "Pressure zz", "Pressure xy"
 values
      1 -0.398408E+0001 -0.398408E+0001 -0.398408E+0001  0.000000E+0000
 -0.559055E+0001 -0.559055E+0001 -0.398408E+0001  0.000000E+0000
 -0.162488E+0001 -0.162488E+0001 -0.398408E+0001  0.000000E+0000
 -0.184127E-0001 -0.184127E-0001 -0.398408E+0001  0.000000E+0000
 end values
 Result "Strain" "MPM"           6 Vector OnGaussPoints "Material_Point"
 ComponentNames "eps xx", "eps yy", "eps xy"
 values
      1   0.000000E+000  -0.163215E-003   0.129528E-004
   0.000000E+000  -0.127827E-003   0.129528E-004
   0.000000E+000  -0.127827E-003   0.483405E-004
   0.000000E+000  -0.163215E-003   0.483405E-004
 end values
 Result "Plastic strain" "MPM"           6 Vector OnGaussPoints "Material_Point"
 ComponentNames "EpsP xx", "EpsP yy", "EpsP zz", "EpsP xy"
 values
      1 -0.800484E+0001 -0.120293E+0002  0.971703E-0001 -0.800484E+0001
 -0.934662E+0001 -0.131056E+0002  0.971703E-0001 -0.800484E+0001
 -0.538095E+0001 -0.913990E+0001  0.362645E+0000 -0.800484E+0001
 -0.403917E+0001 -0.806358E+0001  0.362645E+0000 -0.800484E+0001
 end values
 Result "Failure" "MPM"           6 Scalar OnGaussPoints "Material_Point"
 ComponentNames "Failure Indicator"
 values
      1  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
  0.000000E+0000
 end values
