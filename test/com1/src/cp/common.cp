open System;

object sys : system;
  sys.clock  (18500 kilohz);
  -- sys.clock  (20000 kilohz);
  sys.target ("xc3s1000-ft256-5");
  --sys.target ("xcf04s");
  sys.target ("xc18v04");
  sys.reset_level (0);
