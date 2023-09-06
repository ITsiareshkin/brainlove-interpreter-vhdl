-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2021 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Ivan Tsiareshkin, xtsiar00
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet ROM
   CODE_ADDR : out std_logic_vector(11 downto 0); -- adresa do pameti
   CODE_DATA : in std_logic_vector(7 downto 0);   -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
   CODE_EN   : out std_logic;                     -- povoleni cinnosti
   
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(9 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- ram[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_WREN  : out std_logic;                    -- cteni z pameti (DATA_WREN='0') / zapis do pameti (DATA_WREN='1')
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA obsahuje stisknuty znak klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna pokud IN_VLD='1'
   IN_REQ    : out std_logic;                     -- pozadavek na vstup dat z klavesnice
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- pokud OUT_BUSY='1', LCD je zaneprazdnen, nelze zapisovat,  OUT_WREN musi byt '0'
   OUT_WREN : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0' | guess you mean WREN?
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
		-- Program counter (PC)
		signal pc_inc: std_logic;
		signal pc_dec: std_logic;
		signal pc_reg: std_logic_vector(11 downto 0); -- Same as CODE_ADDR

		-- Pointer (PTR)
		signal ptr_inc: std_logic;
		signal ptr_dec: std_logic;
		signal ptr_reg: std_logic_vector(9 downto 0); -- Same as DATA_ADDR

		-- Counter (CNT)
		signal cnt_inc: std_logic;
		signal cnt_dec: std_logic;
		signal cnt_reg: std_logic_vector(7 downto 0);

		type instructions is (ins_ptr_inc, ins_ptr_dec, ins_val_inc, ins_val_dec, ins_loop_start, ins_loop_end, ins_print, ins_read, ins_loop_break, ins_prog_end, ins_empty);
		signal instruction : instructions;

		type fsm_state is (state_start, state_fetch, state_decode, state_ptr_inc, state_ptr_dec, state_val_inc_1, state_val_inc_2, state_val_dec_1, state_val_dec_2, state_print, state_read,
			state_loop_start, state_loop, state_loop_en,state_loop_end_1, state_loop_end_2, state_loop_end_3, state_loop_end_en, state_loop_break_1, state_loop_break_2, state_end, state_others);
	
		signal p_state: fsm_state := state_start;
		signal n_state: fsm_state;

		signal mux_data_sel: std_logic_vector(1 downto 0) := "11";
		signal mux_data: std_logic_vector(7 downto 0);
begin
		-- Decoder (DC)
		decoder: process (CODE_DATA)
		begin
			case CODE_DATA is
				when X"3E" =>  instruction <= ins_ptr_inc;
				when X"3C" =>  instruction <= ins_ptr_dec;
				when X"2B" =>  instruction <= ins_val_inc;
				when X"2D" =>  instruction <= ins_val_dec;
				when X"5B" =>  instruction <= ins_loop_start;
				when X"5D" =>  instruction <= ins_loop_end;
				when X"2E" =>  instruction <= ins_print;
				when X"2C" =>  instruction <= ins_read;
				when X"7E" =>  instruction <= ins_loop_break;
				when X"00" =>  instruction <= ins_prog_end;
				when others => instruction <= ins_empty;
			end case;
		end process;

		-- PC
		pc_cnt: process (RESET, CLK, pc_inc, pc_dec)
		begin
			if (RESET='1') then
				pc_reg <= (others=>'0');
			elsif (CLK'event) and (CLK='1') then
				if (pc_inc='1') then
					pc_reg <= pc_reg + 1;
				elsif (pc_dec='1') then
					pc_reg <= pc_reg - 1;
				end if;
			end if;
		end process;

		CODE_ADDR <= pc_reg;

		-- PTR
		ptr_cnt: process (RESET, CLK, ptr_inc, ptr_dec)
		begin
			if (RESET='1') then
				ptr_reg <= (others=>'0');
			elsif (CLK'event) and (CLK='1') then
				if (ptr_inc='1') then
					ptr_reg <= ptr_reg + 1;
				elsif (ptr_dec='1') then
					ptr_reg <= ptr_reg - 1;
				end if;
			end if;
		end process;

		DATA_ADDR <= ptr_reg;

		-- CNT
		cnt: process (RESET, CLK, cnt_inc, cnt_dec)
		begin
			if (RESET='1') then
				cnt_reg <= (others => '0');
			elsif (CLK'event) and (CLK='1') then
				if (cnt_inc='1') then
					cnt_reg <= cnt_reg + 1;
				elsif (cnt_dec='1') then
					cnt_reg <= cnt_reg - 1;
				end if;
			end if;
		end process;

		OUT_DATA <= DATA_RDATA;

		-- Mux
		mux: process (RESET, CLK, mux_data_sel)
		begin
			if (RESET='1') then
				mux_data <= (others => '0'); 
			elsif (CLK'event) and (CLK='1') then
				case (mux_data_sel) is
					when "00" => mux_data <= IN_DATA;
					when "01" => mux_data <= DATA_RDATA + 1;
					when "10" => mux_data <= DATA_RDATA - 1;
					when "11" => mux_data <= DATA_RDATA;
					when others => null;
				end case;
			end if;
		end process;

		DATA_WDATA <= mux_data;

		-- FSM state update
		state_update: process (RESET, CLK, EN, n_state)
		begin
			if (RESET='1') then
				p_state <= state_start;
			elsif (CLK'event) and (CLK='1') then
				if (EN='1') then
					p_state <= n_state;
				end if;
			end if;
		end process;

		fsm: process (p_state, OUT_BUSY, IN_VLD, CODE_DATA, cnt_reg, DATA_RDATA, EN, instruction)
		begin
			pc_inc <= '0';
			pc_dec <= '0';

			ptr_inc <= '0';
			ptr_dec <= '0';

			cnt_inc <= '0';
			cnt_dec <= '0';
			
			mux_data_sel <= "11";

			IN_REQ <= '0';
			OUT_WREN <= '0';
			CODE_EN <= '0'; 
			DATA_EN <= '0';
			DATA_WREN <= '0';

			case p_state is
				when state_start =>
					n_state <= state_fetch;

				when state_fetch =>
					CODE_EN <= '1';
					n_state <= state_decode;
				when state_decode =>
					case instruction is
						when ins_ptr_inc => n_state <= state_ptr_inc;
						when ins_ptr_dec => n_state <= state_ptr_dec;
						when ins_val_inc =>
							DATA_EN <= '1';
							DATA_WREN <= '0';
							n_state <= state_val_inc_1;
						when ins_val_dec =>
							DATA_EN <= '1';
							DATA_WREN <= '0';
							n_state <= state_val_dec_1;
						when ins_loop_start =>
							pc_inc <= '1';
							DATA_EN <= '1';
							DATA_WREN <= '0';
							n_state <= state_loop_start;
						when ins_loop_end =>
							DATA_EN <= '1';
							DATA_WREN <= '0';
							n_state <= state_loop_end_1;
						when ins_print =>
							DATA_EN <= '1';
							DATA_WREN <= '0';
							n_state <= state_print;
						when ins_read =>
							IN_REQ <= '1';
							mux_data_sel <= "00";
							n_state <= state_read;
						when ins_loop_break =>
							n_state <= state_loop_break_1;
						when ins_prog_end => n_state <= state_end;
						when others => n_state <= state_others;
					end case;

				when state_ptr_inc =>
					ptr_inc <= '1';
					pc_inc <= '1';
					n_state <= state_fetch;

				when state_ptr_dec =>
					ptr_dec <= '1';
					pc_inc <= '1';
					n_state <= state_fetch;

				when state_val_inc_1 =>
					mux_data_sel <= "01";
					n_state <= state_val_inc_2;
					

				when state_val_inc_2 =>
					DATA_EN <= '1';
					DATA_WREN <= '1';
					pc_inc <= '1';
					n_state <= state_fetch;

				when state_val_dec_1 =>
					mux_data_sel <= "10";
					n_state <= state_val_dec_2;
			
				when state_val_dec_2 =>
					DATA_EN <= '1';
					DATA_WREN <= '1';
					pc_inc <= '1';
					n_state <= state_fetch;

				when state_loop_start =>
					if (DATA_RDATA=(DATA_RDATA'range => '0')) then
						cnt_inc <= '1';
						n_state <= state_loop_en; 
					else
						n_state <= state_fetch;
					end if;

				when state_loop =>
					if cnt_reg = (cnt_reg'range => '0') then
						n_state <= state_fetch;
					elsif (instruction=ins_loop_end) then
						pc_inc <= '1';
						cnt_dec <= '1';
						n_state <= state_loop_en;
					elsif (instruction=ins_loop_start) then
						pc_inc <= '1';
						cnt_inc <= '1';
						n_state <= state_loop_en;
					else
						pc_inc <= '1';
						n_state <= state_loop_en;
					end if;

				when state_loop_en =>
					CODE_EN <= '1';
					n_state <= state_loop;

				when state_loop_end_1 =>
					if DATA_RDATA = (DATA_RDATA'range => '0') then
						pc_inc <= '1';
						n_state <= state_fetch;
					else
						cnt_inc <= '1';
						pc_dec <= '1';						  
						n_state <= state_loop_end_en;
					end if;
	
				when state_loop_end_2 =>
					if cnt_reg = (cnt_reg'range => '0') then
						n_state <= state_fetch;
					else
						if (instruction=ins_loop_end) then
							cnt_inc <= '1';
						elsif (instruction=ins_loop_start) then
							cnt_dec <= '1';
						end if;
							n_state <= state_loop_end_3;
					end if;
		
				when state_loop_end_3 =>
					if cnt_reg = (cnt_reg'range => '0') then 
						pc_inc <= '1'; 
					else 
						pc_dec <= '1'; 
					end if;
					n_state <= state_loop_end_en;
						
				when state_loop_end_en =>
					CODE_EN <= '1';
					n_state <= state_loop_end_2;
				
				when state_print =>
					if (OUT_BUSY='0') then
						OUT_WREN <= '1';
						pc_inc <= '1';
						n_state <= state_fetch;
					end if;

				when state_read =>
					if (IN_VLD='1') then
						DATA_EN <= '1';
						DATA_WREN <= '1';
						pc_inc <= '1';
						n_state <= state_fetch;
					else
						IN_REQ <= '1';
						mux_data_sel <= "00";
						n_state <= state_read;
					end if;

				
				when state_loop_break_1 =>
					CODE_EN <= '1';
					pc_inc <= '1';
					n_state <= state_loop_break_2;

				when state_loop_break_2 =>
					if instruction = ins_loop_end then
						n_state <= state_fetch;
					else 
						n_state <= state_loop_break_1;
					end if;
					
				when state_end =>
					n_state <= state_end;
				
				when state_others =>
					pc_inc <= '1';
					n_state <= state_fetch;
			end case;
		end process;
end behavioral;		