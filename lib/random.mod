--
--      ==================================
--      OOOO   OOOO OOOO  O      O   OOOO
--      O   O  O    O     O     O O  O   O
--      O   O  O    O     O     O O  O   O
--      OOOO   OOOO OOOO  O     OOO  OOOO
--      O   O     O    O  O    O   O O   O
--      O   O     O    O  O    O   O O   O
--      OOOO   OOOO OOOO  OOOO O   O OOOO
--      ==================================
--      BSSLAB, Dr. Stefan Bosse, http://www.bsslab.de
--
--      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
--                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
--                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
--                 OUTSIDE OF THE SOFTWARE SYSTEM.
--
--    $AUTHORS:     Stefan Bosse
--    $INITIAL:     (C) 2006-2009 BSSLAB
--    $CREATED:     29.9.2008
--    $VERSION:     #version
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements random number generator
--
--
--    $ENDOFINFO
--

#version "2.10";

--
-- parameters used, allowed and default values
--
#parameter
begin
  $datawidth[8,10,12,14,16] <= 8;
  $datatype["int","logic"] <= "logic";
  $seed[0 to 0xffff:int] <= 0xffff;
end;

--
-- Supported object methods
--
#methods ($datatype="logic")
begin
  init();
  read(#lhs:logic[$datawidth]);
  seed(#rhs:logic[$datawidth]);
end;
#methods ($datatype="int")
begin
  init();
  read(#lhs:int[$datawidth]);
  seed(#rhs:int[$datawidth]);
end;


--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface($datatype="logic")
begin
  foreach $p in $P.read do
  begin
    signal RND_$O_RE: out std_logic;
    signal RND_$O_RD: in std_logic_vector[$datawidth];
  end;
  foreach $p in $P.seed do
  begin
    signal RND_$O_WE: out std_logic;
    signal RND_$O_WR: out std_logic_vector[$datawidth];
  end;
  foreach $p in $P.init do
  begin
    signal RND_$O_INIT: out std_logic;
  end;
  foreach $p in $P do
  begin
    signal RND_$O_GD: in std_logic;
  end;
end;

#interface($datatype="int")
begin
  foreach $p in $P.read do
  begin
    signal RND_$O_RE: out std_logic;
    signal RND_$O_RD: in signed[$datawidth];
  end;
  foreach $p in $P.seed do
  begin
    signal RND_$O_WE: out std_logic;
    signal RND_$O_WR: out signed[$datawidth];
  end;
  foreach $p in $P.init do
  begin
    signal RND_$O_INIT: out std_logic;
  end;
  foreach $p in $P do
  begin
    signal RND_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.read do
  begin
    RND_$O_RE => RND_$O_$p_RE;
    RND_$O_RD => RND_$O_$p_RD;
  end;
  foreach $p in $P.seed do
  begin
    RND_$O_WE => RND_$O_$p_WE;
    RND_$O_WR => RND_$O_$p_WR;
  end;
  foreach $p in $P.init do
  begin
    RND_$O_INIT => RND_$O_$p_INIT;
  end;
  foreach $p in $P do
  begin
    RND_$O_GD => RND_$O_$p_GD;
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
    RND_$O_INIT <= RND_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for RND_$O_GD = '0';
  end;
end;

read: #access
begin
  #data
  begin
    RND_$O_RE <= RND_$O_GD when $ACC else '0';
    $ARG1 <= RND_$O_RD when $ACC else 0;
  end;
  #control
  begin
    wait for RND_$O_GD = '0';
  end;
end;

seed: #access
begin
  #data
  begin
    RND_$O_WE <= RND_$O_GD when $ACC else '0';
    RND_$O_WR <= $ARG1 when $ACC else 0;
  end;
  #control
  begin
    wait for RND_$O_GD = '0';
  end;
end;

--
-- Implementation (global module context)
-- VHDL signals required, both for mapping processes
-- and auxilliary signals.
--
#signals($datatype="logic")
begin
  foreach $p in $P.read do
  begin
    signal RND_$O_$p_RD: std_logic_vector[$datawidth];
  end;
  foreach $p in $P.seed do
  begin
    signal RND_$O_$p_WR: std_logic_vector[$datawidth];
  end;
end;

#signals($datatype="int")
begin
  foreach $p in $P.read do
  begin
    signal RND_$O_$p_RD: signed[$datawidth];
  end;
  foreach $p in $P.seed do
  begin
    signal RND_$O_$p_WR: signed[$datawidth];
  end;
end;

#signals
begin
  --
  -- Implementation signals
  --
  signal RND_$O_d_in: std_logic;
  signal RND_$O_seed: std_logic_vector[$datawidth];
  signal RND_$O_data_shift: std_logic_vector[$datawidth];
  signal RND_$O_data: std_logic_vector[$datawidth];
  signal RND_$O_shift: std_logic; 
  signal RND_$O_init: std_logic; 
  signal RND_$O_avail: std_logic; 

  foreach $p in $P.read do
  begin
    signal RND_$O_$p_RE: std_logic;
  end;
  foreach $p in $P.seed do
  begin
    signal RND_$O_$p_WE: std_logic;
  end;
  foreach $p in $P.init do
  begin
    signal RND_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P do
  begin
    signal RND_$O_$p_GD: std_logic;    
  end;
end;

#signals ($datawidth=8)
begin
  signal RND_$O_count: std_logic_vector[3];
end;
#signals ($datawidth=10)
begin
  signal RND_$O_count: std_logic_vector[4];
end;
#signals ($datawidth=12)
begin
  signal RND_$O_count: std_logic_vector[4];
end;
#signals ($datawidth=14)
begin
  signal RND_$O_count: std_logic_vector[4];
end;
#signals ($datawidth=16)
begin
  signal RND_$O_count: std_logic_vector[4];
end;

--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
RANDOM_$O_SCHED: #process($datatype="logic")
begin
  if $CLK then
  begin
    if $RES then
    begin
      RND_$O_shift <= '0';
      RND_$O_init <= '0';
      foreach $p in $P do
      begin
        RND_$O_$p_GD <= '1';
      end;
    end
    else
    begin
      foreach $p in $P do
      begin
        RND_$O_$p_GD <= '1';
      end;
      sequence
      begin
        if RND_$O_init = '1' then
        begin
          RND_$O_init <= '0';
        end;
        foreach $p in $P.init do
        begin
          if RND_$O_$p_INIT = '1' then
          begin
            RND_$O_init <= '1';
            RND_$O_seed <= to_logic($seed,$datawidth);
            RND_$O_$p_GD <= '0';
          end;
        end;
        foreach $p in $P.seed do
        begin
          if RND_$O_$p_WE = '1' then
          begin
            RND_$O_init <= '1';
            RND_$O_seed <= RND_$O_$p_WR;
            RND_$O_$p_GD <= '0';
          end;
        end;
        foreach $p in $P.read do
        begin
          if RND_$O_$p_RE = '1' then
          begin
            RND_$O_shift <= '1';
            if RND_$O_avail = '1' then            
            begin
              RND_$O_$p_RD <= RND_$O_data;
              RND_$O_$p_GD <= '0';
              RND_$O_shift <= '0';
            end;
          end;
        end;
      end;
    end;
  end;
end;

RANDOM_$O_SCHED: #process($datatype="int")
begin
  if $CLK then
  begin
    if $RES then
    begin
      RND_$O_shift <= '0';
      RND_$O_init <= '0';
      foreach $p in $P do
      begin
        RND_$O_$p_GD <= '1';
      end;
    end
    else
    begin
      foreach $p in $P do
      begin
        RND_$O_$p_GD <= '1';
      end;
      sequence
      begin
        if RND_$O_init = '1' then
        begin
          RND_$O_init <= '0';
        end;
        foreach $p in $P.init do
        begin
          if RND_$O_$p_INIT = '1' then
          begin
            RND_$O_init <= '1';
            RND_$O_seed <= to_logic($seed,$datawidth);
            RND_$O_$p_GD <= '0';
          end;
        end;
        foreach $p in $P.seed do
        begin
          if RND_$O_$p_WE = '1' then
          begin
            RND_$O_init <= '1';
            RND_$O_seed <= to_logic(RND_$O_$p_WR,$datawidth);
            RND_$O_$p_GD <= '0';
          end;
        end;
        foreach $p in $P.read do
        begin
          if RND_$O_$p_RE = '1' then
          begin
            RND_$O_shift <= '1';
            if RND_$O_avail = '1' then            
            begin
              RND_$O_$p_RD <= to_int(RND_$O_data,$datawidth);
              RND_$O_$p_GD <= '0';
              RND_$O_shift <= '0';
            end;
          end;
        end;
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
    if $RES or RND_$O_init = '1' then
    begin
      RND_$O_data_shift <= RND_$O_seed;
      RND_$O_data <= to_logic(0,8);
      RND_$O_count <= 0b000;
      RND_$O_avail <= '0';
    end
    elsif RND_$O_shift = '1' then
    begin
      RND_$O_data_shift <= RND_$O_d_in & RND_$O_data_shift[7 downto 1]; 
      RND_$O_count <= RND_$O_count + 1;
      if RND_$O_count = 0b111 then
      begin
        RND_$O_avail <= '1';
        RND_$O_data <= RND_$O_data_shift;
        RND_$O_count <= 0b000;
      end
      else
      begin
        RND_$O_avail <= '0';
      end;
    end;
  end;  
end;
RANDOM_$O: #process ($datawidth=10)
begin
  if $CLK then
  begin
    if $RES or RND_$O_init = '1' then
    begin
      RND_$O_data_shift <= RND_$O_seed;
      RND_$O_data <= to_logic(0,10);
      RND_$O_count <= 0b0000;
      RND_$O_avail <= '0';
    end
    elsif RND_$O_shift = '1' then
    begin
      RND_$O_data_shift <= RND_$O_d_in & RND_$O_data_shift[9 downto 1]; 
      RND_$O_count <= RND_$O_count + 1;
      if RND_$O_count = 0b1001 then
      begin
        RND_$O_avail <= '1';
        RND_$O_data <= RND_$O_data_shift;
        RND_$O_count <= 0b0000;
      end
      else
      begin
        RND_$O_avail <= '0';
      end;
    end;
  end;  
end;
RANDOM_$O: #process ($datawidth=12)
begin
  if $CLK then
  begin
    if $RES or RND_$O_init = '1' then
    begin
      RND_$O_data_shift <= RND_$O_seed;
      RND_$O_data <= to_logic(0,12);
      RND_$O_count <= 0b0000;
      RND_$O_avail <= '0';
    end
    elsif RND_$O_shift = '1' then
    begin
      RND_$O_data_shift <= RND_$O_d_in & RND_$O_data_shift[11 downto 1]; 
      RND_$O_count <= RND_$O_count + 1;
      if RND_$O_count = 0b1011 then
      begin
        RND_$O_avail <= '1';
        RND_$O_data <= RND_$O_data_shift;
      end
      else
      begin
        RND_$O_avail <= '0';
      end;
    end;
  end;  
end;
RANDOM_$O: #process ($datawidth=14)
begin
  if $CLK then
  begin
    if $RES or RND_$O_init = '1' then
    begin
      RND_$O_data_shift <= RND_$O_seed;
      RND_$O_data <= to_logic(0,14);
      RND_$O_count <= 0b0000;
      RND_$O_avail <= '0';
    end
    elsif RND_$O_shift = '1' then
    begin
      RND_$O_data_shift <= RND_$O_d_in & RND_$O_data_shift[13 downto 1]; 
      RND_$O_count <= RND_$O_count + 1;
      if RND_$O_count = 0b1101 then
      begin
        RND_$O_avail <= '1';
        RND_$O_data <= RND_$O_data_shift;
        RND_$O_count <= 0b0000;
      end
      else
      begin
        RND_$O_avail <= '0';
      end;
    end;
  end;  
end;
RANDOM_$O: #process ($datawidth=16)
begin
  if $CLK then
  begin
    if $RES or RND_$O_init = '1' then
    begin
      RND_$O_data_shift <= RND_$O_seed;
      RND_$O_data <= to_logic(0,16);
      RND_$O_count <= 0b0000;
      RND_$O_avail <= '0';
    end
    elsif RND_$O_shift = '1' then
    begin
      RND_$O_data_shift <= RND_$O_d_in & RND_$O_data_shift[13 downto 1]; 
      RND_$O_count <= RND_$O_count + 1;
      if RND_$O_count = 0b1111 then
      begin
        RND_$O_avail <= '1';
        RND_$O_data <= RND_$O_data_shift;
        RND_$O_count <= 0b0000;
      end
      else
      begin
        RND_$O_avail <= '0';
      end;
    end;
  end;  
end;

--
-- Implementation, toplevel statements
--
#top ($datawidth=8)
begin
  RND_$O_d_in <= RND_$O_data_shift[0] xor RND_$O_data_shift[4] xor RND_$O_data_shift[5] xor RND_$O_data_shift[7];
end;
#top ($datawidth=10)
begin
  RND_$O_d_in <= RND_$O_data_shift[2] xor RND_$O_data_shift[9];
end;
#top ($datawidth=12)
begin
  RND_$O_d_in <= RND_$O_data_shift[2] xor RND_$O_data_shift[3] xor RND_$O_data_shift[6] xor RND_$O_data_shift[11];
end;
#top ($datawidth=14)
begin
  RND_$O_d_in <= RND_$O_data_shift[0] xor RND_$O_data_shift[10] xor RND_$O_data_shift[11] xor RND_$O_data_shift[13];
end;
#top ($datawidth=16)
begin
  RND_$O_d_in <= RND_$O_data_shift[1] xor RND_$O_data_shift[2] xor RND_$O_data_shift[4] xor RND_$O_data_shift[15];
end;
