-------------------------------------------------------------------------------
--
-- Title       : block_ram
-- Design      : Micro
-- Author      : 
-- Company     : 
--
-------------------------------------------------------------------------------
--
-- File        : C:\Projects\Micro\VHDL\Micro\src\TOP\Core\block_ram.vhd
-- Generated   : Thu Dec 11 12:56:28 2014
-- From        : interface description file
-- By          : Itf2Vhdl ver. 1.22
--
-------------------------------------------------------------------------------
--
-- Description : 
--
-------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.STD_LOGIC_ARITH.all;

--{{ Section below this comment is automatically maintained
--   and may be overwritten
--{entity {block_ram} architecture {block_ram}}

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity block_ram is	
	generic(
		mem_depth : positive := 12; -- {5 => depth=2^5, 6=> depth=2^6...}
		mem_width :positive := 16 -- width of data bits!
		);
	 port(
		 Reset : in BOOLEAN;
		 clk : in STD_LOGIC;
		 write : in STD_LOGIC;
		 wr_addr : in STD_LOGIC_VECTOR(mem_depth-1 downto 0);
		 wr_data : in STD_LOGIC_VECTOR(mem_width-1 downto 0);
		 read : in STD_LOGIC;
		 rd_addr : in STD_LOGIC_VECTOR(mem_depth-1 downto 0);
		 rd_data : out STD_LOGIC_VECTOR(mem_width-1 downto 0)
	     );
end block_ram;

--}} End of automatically maintained section

architecture block_ram of block_ram is	

	type memory_type is array(0 to ((2**mem_depth)-1)) of integer range 0 to ((2**mem_width)-1);

	-- init memory
	function init_ram
		return memory_type is 
		variable tmpD : memory_type := (others => (0));
	begin 
		if (mem_width=32) then 
			tmpD(1023) := 1043;			-- version 410	 
		elsif (mem_width=10) then -- init gamma lut
			for id in 0 to 1023 loop
				tmpD(id) := 64; --"0001000000"; -- gain of 1 --CONV_STD_LOGIC_VECTOR(id,10);	
			end loop;			
		else
			tmpD := (others => (0));
		end if;
		
		return tmpD;
	end init_ram;	 

	signal mem : memory_type := init_ram;  	 
	
	attribute syn_ramstyle : string;
	attribute syn_ramstyle of mem : signal is "no_rw_check";   
	
	signal 	iwr_addr : integer range 0 to ((2**mem_depth)-1);
	signal 	ird_addr : integer range 0 to ((2**mem_depth)-1); 
begin

	-- enter your statements here --		
-------------------------------------------------	
-- write data to memory
-------------------------------------------------	
	mem_write_proc: process(clk)	 
	begin  							  
		if (rising_edge(clk)) then 
			if (write='1') then
				mem(iwr_addr) <= CONV_INTEGER(wr_data);
			end if;
		end if;
	end process;  
	iwr_addr <= CONV_INTEGER(wr_addr);

-------------------------------------------------	
-- read data from memory
-------------------------------------------------	
	mem_read_proc: process(clk)	 
	begin  
		if (rising_edge(clk)) then 
			if (read='1') then
				rd_data <= CONV_STD_LOGIC_VECTOR(mem(ird_addr),mem_width);
			end if;
		end if;
	end process;	
	ird_addr <= CONV_INTEGER(rd_addr);

end block_ram;
