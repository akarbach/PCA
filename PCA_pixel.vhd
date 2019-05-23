library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;        -- for addition & counting
use ieee.numeric_std.all;               -- for type conversions
use ieee.std_logic_arith.all;
library work;
use work.types_packege.all;

entity PCA_pixel is
    generic (
        number_output_features_g : positive := 128;
        mem_depth                : positive := 6;
        mem_width                : positive := 8
    );
    port (
        reset          : in  std_logic;
        clock          : in  std_logic;
        sof            : in  std_logic;
        eof            : in  std_logic;
        data_in        : in  std_logic_vector(7 downto 0);
        data_in_valid  : in  std_logic;
        --weight_in      : in  mat(0 to number_output_features_g - 1)(0 to number_output_features_g - 1);
        data_out       : out vec(0 to number_output_features_g - 1);
        data_out_valid : out std_logic
    ) ;
end PCA_pixel;

architecture PCA_pixel_arc of PCA_pixel is

component block_ram
  generic(
       mem_depth : positive := 12;
       mem_width : positive := 16
  );
  port (
       Reset   : in  boolean;
       clk     : in  std_logic;
       rd_addr : in  std_logic_vector(mem_depth-1 downto 0);
       read    : in  std_logic;
       wr_addr : in  std_logic_vector(mem_depth-1 downto 0);
       wr_data : in  std_logic_vector(mem_width-1 downto 0);
       write   : in  std_logic;
       rd_data : out std_logic_vector(mem_width-1 downto 0)
  );
end component;
    --type vec is array (natural range <>) of std_logic_vector(7 downto 0);



    signal partial_sums : vec (0 to number_output_features_g - 1);
    signal weight_in    : vec (0 to number_output_features_g - 1);
    signal index        : natural range 0 to number_output_features_g - 1;
    signal data_out_int : std_logic_vector(7 + number_output_features_g downto 0);
    signal reset_bool   : boolean;

    signal rd_addr      : std_logic_vector(number_output_features_g * mem_depth-1 downto 0);
    signal rd           : std_logic := '1';
    signal wr_addr      : std_logic_vector(number_output_features_g * mem_depth-1 downto 0);
    signal wr_data      : std_logic_vector(number_output_features_g * mem_width-1 downto 0);
    signal wr           : std_logic;
    signal rd_data      : std_logic_vector(number_output_features_g * mem_width-1 downto 0);
begin

    -- multiply 
    process(reset, clock)
        variable temp_mult : std_logic_vector(15 downto 0);
    begin
        if reset = '1' then
            partial_sums <= (others => (others => '0'));
            index        <= 0;
            --rd           <= '0';
            wr           <= '0';
        elsif rising_edge(clock) then
            data_out_valid <= '0';
            --rd             <= '1'; -- always read. the address will only change.

            if sof = '1' then
                index <= 0;
                partial_sums <= (others => (others => '0'));
            elsif eof = '1' then
                for i in 0 to (number_output_features_g - 1) loop
                    data_out(i) <= partial_sums(i)(partial_sums(i)'left downto partial_sums(i)'left - 7);
                end loop;
                data_out_valid <= '1';
            end if;

            if data_in_valid = '1' then
                for i in 0 to (number_output_features_g - 1) loop
                    temp_mult       := data_in * weight_in(i);
                    partial_sums(i) <= temp_mult(15 downto 8) + partial_sums(i);
                end loop;
                index <= index + 1;
                if index = (number_output_features_g - 1) then
                    index <= 0;
                end if;
            end if;
        end if;
    end process;

    rd_addr <= conv_std_logic_vector(index, number_output_features_g * mem_depth);

    process(rd_data)
    begin
        for i in 0 to (number_output_features_g - 1) loop
            weight_in(i) <= rd_data(8 * (1 + i) - 1 downto 8 * i);
        end loop;
    end process;

    reset_bool <= reset = '1'; 
    rd <= '1';
    mem_gen: for I in 0 to (number_output_features_g - 1) generate
        mem_I : block_ram
        generic map(
           mem_depth => mem_depth,
           mem_width => mem_width
        )
        port map(
           Reset   => reset_bool,
           clk     => clock,
           rd_addr => rd_addr((mem_depth * (1 + I) - 1) downto (I * mem_depth)),
           rd_data => rd_data((mem_width * (1 + I) - 1) downto (I * mem_width)),
           read    => rd,
           wr_addr => wr_addr((mem_depth * (1 + I) - 1) downto (I * mem_depth)),
           wr_data => wr_data((mem_width * (1 + I) - 1) downto (I * mem_width)),
           write   => wr
        );
    end generate;

end  PCA_pixel_arc;
