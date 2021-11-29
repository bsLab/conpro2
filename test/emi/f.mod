--
-- Implements random number generator
--

--
-- parameters used, allowed and default values
--
#parameter
begin
  $datawidth[8,10] <= 8;
  -- $datawidth[8 to 12] <= 8;
  $time[0 to 4095] <= 100;
  $myreg;
end;

--
-- Supported object methods
--
#methods
begin
  init();
  read(#lhs:logic[$datawidth]);
  time(#rhs:natural);
  set(#rhs:logic[$datawidth]);
  -- read2(#lhs:logic[$datawidth],#rhs:logic[$addrwidth]);
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.read do
  begin
    signal F_$O_RE: out std_logic;
    signal F_$O_RD: in std_logic_vector[$datawidth];
  end;
  foreach $p in $P.init do
  begin
    signal F_$O_INIT: out std_logic;
  end;
  foreach $p in $P do
  begin
    signal F_$O_GD: in std_logic;
  end;
  foreach $p in $P.time do
  begin
    signal F_$O_WE: out std_logic;
    signal F_$O_TIME_SET: out logic[index_width($time)];
  end;

end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.read do
  begin
    F_$O_RE => F_$O_$p_RE;
    F_$O_RD => F_$O_$p_RD;
  end;
  foreach $p in $P.init do
  begin
    F_$O_INIT => F_$O_$p_INIT;
  end;
  foreach $p in $P do
  begin
    F_$O_GD => F_$O_$p_GD;
  end;
  foreach $p in $P.time do
  begin
    F_$O_WE => F_$O_$p_WE;
    F_$O_TIME_SET => F_$O_$p_TIME_SET;
  end;
end;

--
-- Object method access (local process context)
-- for each method ...
--
init: #access
begin
  #data
  begin
    F_$O_INIT <= '1' when $ACC else '0';
  end;
  #control
  begin
    null;
  end;
end;

read: #access
begin
  #data
  begin
    F_$O_RE <= '1' when $ACC else '0';
    $ARG1 <= F_$O_RD when $ACC else 0;
  end;
  #control
  begin
    wait for F_$O_GD = '1';
  end;
end;

--
-- Set parameter value // toplevel only
--
time: #access
begin
  #set
  begin
    $time <= $ARG1;
  end;
  #data
  begin
    F_$O_WE <= '1' when $ACC else '0';
    F_$O_TIME_SET <= index($time,$ARG1) when $ACC else 0;
  end;
  #control
  begin
    null;
  end;
end;

set: #access
begin
  #set
  begin
    $myreg <= $ARG1;
  end;
end;

--
-- Implementation (global module context)
-- VHDL signals required, both for mapping processes
-- and auxilliary signals.
--
#signals
begin
  --
  -- Implementation signals
  --
  signal F_$O_d_in: std_logic;
  signal F_$O_data_shift: std_logic_vector[$datawidth];
  signal F_$O_data: std_logic_vector[$datawidth];
  signal F_$O_shift: std_logic; 
  signal F_$O_init: std_logic; 
  signal F_avail: std_logic; 

  foreach $p in $P.read do
  begin
    signal F_$O_$p_RE: std_logic;
    signal F_$O_$p_RD: std_logic_vector[$datawidth];
  end;
  foreach $p in $P.init do
  begin
    signal F_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P do
  begin
    signal F_$O_$p_GD: std_logic;    
  end;
  foreach $p in $P.time do
  begin
    signal F_$O_$p_TIME_SET: logic[index_width($time)];    
  end;

  constant v1: integer := 32;
  constant V;
  type command;
  
  -- type command;
  
  type pro_states is {
    S_pat_1_start, -- PROCESS0[:0]
    S_i1_fun, -- FUN52868[pat.cp:14]
    S_i2_assign, -- ASSIGN24852[pat.cp:16]
    S_i3_assign, -- ASSIGN56314[pat.cp:17]
    S_i5_branch, -- BRANCH71439[pat.cp:21]
    S_i6_for_loop -- COUNT_LOOP95870[pat.cp:23]
  };  

  type ARRAY_pat_counter_TYPE is array[0 to 3]
    of std_logic_vector[11 downto 0];

  signal ARRAY_pat_counter: ARRAY_pat_counter_TYPE;

  foreach $p in $P do
  begin
    type ARRAY_pat_$p_counter_TYPE is array[0 to 3]
        of std_logic_vector[11 downto 0];
  end;  

  signal F_$O_timer : std_logic_vector[width($time)];
end;
#signals ($datawidth=8)
begin
  signal F_$O_count: std_logic_vector[3];
end;
#signals ($datawidth=10)
begin
  signal F_$O_count: std_logic_vector[4];
end;

RANDOM:#process
begin
  case cmd is
  begin
    when to_logic(REQ,8) => x <= '1';
    when to_logic(REP,8) => x <= '0';
  end;
  case pro_state is                           
  begin     
    when S_p1_start => -- PROCESS0[:0]
      null;
    when S_i1_for_loop => -- COUNT_LOOP60167[t.cp:19]
      LOOP_i_0 <= CONST_I5_1;                        
    when S_i1_for_loop_cond => -- COUNT_LOOP60167[t.cp:19]
    begin
      LOOP_i_0 <= to_logic(v1,8);                                               
      LOOP_i_0 <= to_logic(V,8);                                               
      LOOP_i_0 <= to_int(v1,8);                                               
    end;
    when S_i2_fun => -- FUN7100[t.cp:21]
      null;                             
    when S_i3_bind_to_4 => -- ASSIGN21571[t.cp:24]
      null;
    when S_i1_for_loop_incr => -- COUNT_LOOP60167[t.cp:19]
      LOOP_i_0 <= LOOP_i_0 + CONST_I5_1;                  
    when S_p1_end => -- PROCESS0[:0]    
      null;                         
  end;
end;
--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
RANDOM_$O_SCHED: #process
begin
  if $CLK then
  begin
    if $RES then
    begin
      F_$O_shift <= '0';
      F_$O_init <= '0';
      foreach $p in $P do
      begin
        F_$O_$p_GD <= '1';
      end;
    end
    else
    begin
      foreach $p in $P do
      begin
        F_$O_$p_GD <= '1';
      end;
      sequence
      begin
        if F_$O_init = '1' then
        begin
          F_$O_init <= '0';
        end;
        foreach $p in $P.init do
        begin
          if F_$O_$p_INIT = '1' then
          begin
            F_$O_init <= '1';
            F_$O_$p_GD <= '0';
          end;
        end;
        foreach $p in $P.read do
        begin
          if F_$O_$p_RE = '1' then
          begin
            F_$O_shift <= '1';
            if F_$O_avail = '1' then            
            begin
              F_$O_$p_RD <= F_$O_data;
              F_$O_$p_GD <= '0';
              F_$O_shift <= '0';
            end;
          end;
        end;
        if others then F_$O_shift <= '0'; 
      end;
    end;
  end;
end;

--
-- Some more implementation processes
--

RANDOM_$O: #process ($datawidth=8)
begin
  if $CLK then
  begin
    if $RES or F_$O_init = '1' then
    begin
      F_$O_data_shift <= 0b11111111;
      F_$O_data <= (others => '0');
      F_$O_count <= 0b000;
      F_$O_avail <= '0';
    end
    elsif F_$O_shift = '1' then
    begin
      F_$O_data_shift <= F_$O_d_in & F_$O_data_shift[7 downto 1]; 
      F_$O_count <= F_$O_count + 1;
      if F_$O_count = 0b111 then
      begin
        F_$O_avail <= '1';
        F_$O_data <= F_$O_data_shift;
      end
      else
      begin
        F_$O_avail <= '0';
      end;
    end;
  end;  
end;
RANDOM_$O: #process ($datawidth=10)
begin
  if $CLK then
  begin
    if $RES or F_$O_init = '1' then
    begin
      F_$O_data_shift <= 0b1111111111;
      F_$O_data <= $myreg;
      F_$O_count[F_$O_count'high downto F_$O_count'low] <= 0b0000;
      for i = F_$O_count'high downto F_$O_count'low do
      begin
       F_$O_count[i] <= '0';
      end;
      F_$O_avail <= '0';
    end
    elsif F_$O_shift = '1' then
    begin
      F_$O_data_shift <= F_$O_d_in & F_$O_data_shift[9 downto 1]; 
      F_$O_count <= F_$O_count + 1;
      if F_$O_count = 0b1001 then
      begin
        F_$O_avail <= '1';
        F_$O_data <= F_$O_data_shift;
      end
      else
      begin
        F_$O_avail <= '0';
      end;
    end;
  end;  
end;


TIMER_$O: #process
begin
  sequence
  begin
    foreach $this_time in $time do
    begin
      if F_$O_TIME_SET = index($time,$this_time) then
      begin
        F_$O_timer <= to_logic(($this_time * 100),16);
      end;
    end;
  end;
end;

--
-- Implementation, toplevel statements
--
#top ($datawidth=8)
begin
  F_$O_d_in <= F_$O_data_shift[0] xor F_$O_data_shift[4] xor F_$O_data_shift[5] xor F_$O_data_shift[7];
end;
#top ($datawidth=10)
begin
  F_$O_d_in <= F_$O_data_shift[2] xor F_$O_data_shift[9];
end;
