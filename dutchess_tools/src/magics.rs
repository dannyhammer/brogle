use dutchess_core::Tile;

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct MagicEntry {
    pub(crate) mask: u64,
    pub(crate) magic: u64,
    pub(crate) shift: u8,
    pub(crate) offset: usize,
}

#[rustfmt::skip]
pub const ROOK_MAGICS: &[MagicEntry; Tile::COUNT] = &[
  MagicEntry { mask: 0x000101010101017E, magic: 0x0080012080400110, shift: 52, offset: 0 },
  MagicEntry { mask: 0x000202020202027C, magic: 0x0040051000446000, shift: 53, offset: 4096 },
  MagicEntry { mask: 0x000404040404047A, magic: 0x02000A02C0208010, shift: 53, offset: 6144 },
  MagicEntry { mask: 0x0008080808080876, magic: 0x0080045000880082, shift: 53, offset: 8192 },
  MagicEntry { mask: 0x001010101010106E, magic: 0x520010200E000824, shift: 53, offset: 10240 },
  MagicEntry { mask: 0x002020202020205E, magic: 0x0080040022008009, shift: 53, offset: 12288 },
  MagicEntry { mask: 0x004040404040403E, magic: 0x2200008402000908, shift: 53, offset: 14336 },
  MagicEntry { mask: 0x008080808080807E, magic: 0x4080010000224080, shift: 52, offset: 16384 },
  MagicEntry { mask: 0x0001010101017E00, magic: 0x0004802040018000, shift: 53, offset: 20480 },
  MagicEntry { mask: 0x0002020202027C00, magic: 0x60804005E0100840, shift: 54, offset: 22528 },
  MagicEntry { mask: 0x0004040404047A00, magic: 0x5000802001500480, shift: 54, offset: 23552 },
  MagicEntry { mask: 0x0008080808087600, magic: 0x0005002010010018, shift: 54, offset: 24576 },
  MagicEntry { mask: 0x0010101010106E00, magic: 0x0102002004120008, shift: 54, offset: 25600 },
  MagicEntry { mask: 0x0020202020205E00, magic: 0x0001000218040100, shift: 54, offset: 26624 },
  MagicEntry { mask: 0x0040404040403E00, magic: 0x4004800200800100, shift: 54, offset: 27648 },
  MagicEntry { mask: 0x0080808080807E00, magic: 0x0101000100006886, shift: 53, offset: 28672 },
  MagicEntry { mask: 0x00010101017E0100, magic: 0x0080004010402004, shift: 53, offset: 30720 },
  MagicEntry { mask: 0x00020202027C0200, magic: 0x002080804002A000, shift: 54, offset: 32768 },
  MagicEntry { mask: 0x00040404047A0400, magic: 0x0099010020001040, shift: 54, offset: 33792 },
  MagicEntry { mask: 0x0008080808760800, magic: 0x20108A0012006040, shift: 54, offset: 34816 },
  MagicEntry { mask: 0x00101010106E1000, magic: 0x9402020020081014, shift: 54, offset: 35840 },
  MagicEntry { mask: 0x00202020205E2000, magic: 0x0001080110204004, shift: 54, offset: 36864 },
  MagicEntry { mask: 0x00404040403E4000, magic: 0x0080440001104802, shift: 54, offset: 37888 },
  MagicEntry { mask: 0x00808080807E8000, magic: 0x0020020000804401, shift: 53, offset: 38912 },
  MagicEntry { mask: 0x000101017E010100, magic: 0x0000400080012080, shift: 53, offset: 40960 },
  MagicEntry { mask: 0x000202027C020200, magic: 0xD04081020022004C, shift: 54, offset: 43008 },
  MagicEntry { mask: 0x000404047A040400, magic: 0x001A114500200101, shift: 54, offset: 44032 },
  MagicEntry { mask: 0x0008080876080800, magic: 0x0000100100200900, shift: 54, offset: 45056 },
  MagicEntry { mask: 0x001010106E101000, magic: 0x8402002200081044, shift: 54, offset: 46080 },
  MagicEntry { mask: 0x002020205E202000, magic: 0x8005000F00480400, shift: 54, offset: 47104 },
  MagicEntry { mask: 0x004040403E404000, magic: 0x1070880400108601, shift: 54, offset: 48128 },
  MagicEntry { mask: 0x008080807E808000, magic: 0x000000420010830C, shift: 53, offset: 49152 },
  MagicEntry { mask: 0x0001017E01010100, magic: 0x008002200C4002C0, shift: 53, offset: 51200 },
  MagicEntry { mask: 0x0002027C02020200, magic: 0x0090006000404000, shift: 54, offset: 53248 },
  MagicEntry { mask: 0x0004047A04040400, magic: 0x0010008430802000, shift: 54, offset: 54272 },
  MagicEntry { mask: 0x0008087608080800, magic: 0x0009000861001000, shift: 54, offset: 55296 },
  MagicEntry { mask: 0x0010106E10101000, magic: 0x8461800400800800, shift: 54, offset: 56320 },
  MagicEntry { mask: 0x0020205E20202000, magic: 0x0211802200800400, shift: 54, offset: 57344 },
  MagicEntry { mask: 0x0040403E40404000, magic: 0x0005010224005018, shift: 54, offset: 58368 },
  MagicEntry { mask: 0x0080807E80808000, magic: 0x80000044020000A1, shift: 53, offset: 59392 },
  MagicEntry { mask: 0x00017E0101010100, magic: 0x2A00800840008020, shift: 53, offset: 61440 },
  MagicEntry { mask: 0x00027C0202020200, magic: 0x0018500220004001, shift: 54, offset: 63488 },
  MagicEntry { mask: 0x00047A0404040400, magic: 0x00410011A0010040, shift: 54, offset: 64512 },
  MagicEntry { mask: 0x0008760808080800, magic: 0x0424100008008080, shift: 54, offset: 65536 },
  MagicEntry { mask: 0x00106E1010101000, magic: 0x0008001100090044, shift: 54, offset: 66560 },
  MagicEntry { mask: 0x00205E2020202000, magic: 0xC300020004008080, shift: 54, offset: 67584 },
  MagicEntry { mask: 0x00403E4040404000, magic: 0x0001002E00290024, shift: 54, offset: 68608 },
  MagicEntry { mask: 0x00807E8080808000, magic: 0x10A018804502000C, shift: 53, offset: 69632 },
  MagicEntry { mask: 0x007E010101010100, magic: 0x06A1002040800500, shift: 53, offset: 71680 },
  MagicEntry { mask: 0x007C020202020200, magic: 0x040A011342608200, shift: 54, offset: 73728 },
  MagicEntry { mask: 0x007A040404040400, magic: 0x2000408020D60200, shift: 54, offset: 74752 },
  MagicEntry { mask: 0x0076080808080800, magic: 0x4140205900100100, shift: 54, offset: 75776 },
  MagicEntry { mask: 0x006E101010101000, magic: 0x0002480010150100, shift: 54, offset: 76800 },
  MagicEntry { mask: 0x005E202020202000, magic: 0x0992000204008080, shift: 54, offset: 77824 },
  MagicEntry { mask: 0x003E404040404000, magic: 0x0401080106100400, shift: 54, offset: 78848 },
  MagicEntry { mask: 0x007E808080808000, magic: 0x0049800900014480, shift: 53, offset: 79872 },
  MagicEntry { mask: 0x7E01010101010100, magic: 0x3080001100402081, shift: 52, offset: 81920 },
  MagicEntry { mask: 0x7C02020202020200, magic: 0x2001041060804001, shift: 53, offset: 86016 },
  MagicEntry { mask: 0x7A04040404040400, magic: 0x080080224200111A, shift: 53, offset: 88064 },
  MagicEntry { mask: 0x7608080808080800, magic: 0x4021002860100005, shift: 53, offset: 90112 },
  MagicEntry { mask: 0x6E10101010101000, magic: 0x0022000820100402, shift: 53, offset: 92160 },
  MagicEntry { mask: 0x5E20202020202000, magic: 0x5001001822040091, shift: 53, offset: 94208 },
  MagicEntry { mask: 0x3E40404040404000, magic: 0x10A010020808811C, shift: 53, offset: 96256 },
  MagicEntry { mask: 0x7E80808080808000, magic: 0x00100140A0840102, shift: 52, offset: 98304 },
];
pub const ROOK_TABLE_SIZE: usize = 102400;

#[rustfmt::skip]
pub const BISHOP_MAGICS: &[MagicEntry; Tile::COUNT] = &[
  MagicEntry { mask: 0x0040201008040200, magic: 0x0840020884068880, shift: 58, offset: 0 },
  MagicEntry { mask: 0x0000402010080400, magic: 0x80201840810042A2, shift: 59, offset: 64 },
  MagicEntry { mask: 0x0000004020100A00, magic: 0x0209084101002100, shift: 59, offset: 96 },
  MagicEntry { mask: 0x0000000040221400, magic: 0x20082081A0001006, shift: 59, offset: 128 },
  MagicEntry { mask: 0x0000000002442800, magic: 0x0010882000000080, shift: 59, offset: 160 },
  MagicEntry { mask: 0x0000000204085000, magic: 0x0001246020000608, shift: 59, offset: 192 },
  MagicEntry { mask: 0x0000020408102000, magic: 0x2402841008040000, shift: 59, offset: 224 },
  MagicEntry { mask: 0x0002040810204000, magic: 0x0041040180880880, shift: 58, offset: 256 },
  MagicEntry { mask: 0x0020100804020000, magic: 0x00000C0404242400, shift: 59, offset: 320 },
  MagicEntry { mask: 0x0040201008040000, magic: 0x0E0050060242D201, shift: 59, offset: 352 },
  MagicEntry { mask: 0x00004020100A0000, magic: 0x0000440322060401, shift: 59, offset: 384 },
  MagicEntry { mask: 0x0000004022140000, magic: 0x1010040400810180, shift: 59, offset: 416 },
  MagicEntry { mask: 0x0000000244280000, magic: 0x0200040504050000, shift: 59, offset: 448 },
  MagicEntry { mask: 0x0000020408500000, magic: 0x0802042220504000, shift: 59, offset: 480 },
  MagicEntry { mask: 0x0002040810200000, magic: 0x4001808888A01080, shift: 59, offset: 512 },
  MagicEntry { mask: 0x0004081020400000, magic: 0x0000820101081E00, shift: 59, offset: 544 },
  MagicEntry { mask: 0x0010080402000200, magic: 0x0060104808011801, shift: 59, offset: 576 },
  MagicEntry { mask: 0x0020100804000400, magic: 0x21021284A4080210, shift: 59, offset: 608 },
  MagicEntry { mask: 0x004020100A000A00, magic: 0x2018001048004050, shift: 57, offset: 640 },
  MagicEntry { mask: 0x0000402214001400, magic: 0x42A401080341B021, shift: 57, offset: 768 },
  MagicEntry { mask: 0x0000024428002800, magic: 0x0024000080E00504, shift: 57, offset: 896 },
  MagicEntry { mask: 0x0002040850005000, magic: 0x020A000B00A21100, shift: 57, offset: 1024 },
  MagicEntry { mask: 0x0004081020002000, magic: 0x0C04080082080A00, shift: 59, offset: 1152 },
  MagicEntry { mask: 0x0008102040004000, magic: 0x1011008200808480, shift: 59, offset: 1184 },
  MagicEntry { mask: 0x0008040200020400, magic: 0x80041E0021200400, shift: 59, offset: 1216 },
  MagicEntry { mask: 0x0010080400040800, magic: 0x02820200211C2400, shift: 59, offset: 1248 },
  MagicEntry { mask: 0x0020100A000A1000, magic: 0x0141012010040020, shift: 57, offset: 1280 },
  MagicEntry { mask: 0x0040221400142200, magic: 0x0006080004004208, shift: 55, offset: 1408 },
  MagicEntry { mask: 0x0002442800284400, magic: 0x0041080405004000, shift: 55, offset: 1920 },
  MagicEntry { mask: 0x0004085000500800, magic: 0x9002020022480202, shift: 57, offset: 2432 },
  MagicEntry { mask: 0x0008102000201000, magic: 0x2004040400922100, shift: 59, offset: 2560 },
  MagicEntry { mask: 0x0010204000402000, magic: 0x0600831102010088, shift: 59, offset: 2592 },
  MagicEntry { mask: 0x0004020002040800, magic: 0x2044040480222000, shift: 59, offset: 2624 },
  MagicEntry { mask: 0x0008040004081000, magic: 0x0008025F01180800, shift: 59, offset: 2656 },
  MagicEntry { mask: 0x00100A000A102000, magic: 0x0006846080900080, shift: 57, offset: 2688 },
  MagicEntry { mask: 0x0022140014224000, magic: 0x0082200802050150, shift: 55, offset: 2816 },
  MagicEntry { mask: 0x0044280028440200, magic: 0x4068022400064100, shift: 55, offset: 3328 },
  MagicEntry { mask: 0x0008500050080400, magic: 0x08208A8604070100, shift: 57, offset: 3840 },
  MagicEntry { mask: 0x0010200020100800, magic: 0x0887070401830402, shift: 59, offset: 3968 },
  MagicEntry { mask: 0x0020400040201000, magic: 0x4282040A30004208, shift: 59, offset: 4000 },
  MagicEntry { mask: 0x0002000204081000, magic: 0x0908242208002010, shift: 59, offset: 4032 },
  MagicEntry { mask: 0x0004000408102000, magic: 0x000042100C003000, shift: 59, offset: 4064 },
  MagicEntry { mask: 0x000A000A10204000, magic: 0x0081040022010402, shift: 57, offset: 4096 },
  MagicEntry { mask: 0x0014001422400000, magic: 0x0804212018000100, shift: 57, offset: 4224 },
  MagicEntry { mask: 0x0028002844020000, magic: 0x86004C490C000200, shift: 57, offset: 4352 },
  MagicEntry { mask: 0x0050005008040200, magic: 0x0005200815800040, shift: 57, offset: 4480 },
  MagicEntry { mask: 0x0020002010080400, magic: 0x0202120421004400, shift: 59, offset: 4608 },
  MagicEntry { mask: 0x0040004020100800, magic: 0x1048650400202088, shift: 59, offset: 4640 },
  MagicEntry { mask: 0x0000020408102000, magic: 0x001400A630112116, shift: 59, offset: 4672 },
  MagicEntry { mask: 0x0000040810204000, magic: 0x0420410090100082, shift: 59, offset: 4704 },
  MagicEntry { mask: 0x00000A1020400000, magic: 0x2C00020201040200, shift: 59, offset: 4736 },
  MagicEntry { mask: 0x0000142240000000, magic: 0x0400094042020000, shift: 59, offset: 4768 },
  MagicEntry { mask: 0x0000284402000000, magic: 0x0009241082022144, shift: 59, offset: 4800 },
  MagicEntry { mask: 0x0000500804020000, magic: 0x10C0040408020802, shift: 59, offset: 4832 },
  MagicEntry { mask: 0x0000201008040200, magic: 0x0420020401040200, shift: 59, offset: 4864 },
  MagicEntry { mask: 0x0000402010080400, magic: 0x0060010409044220, shift: 59, offset: 4896 },
  MagicEntry { mask: 0x0002040810204000, magic: 0x2000440401081640, shift: 58, offset: 4928 },
  MagicEntry { mask: 0x0004081020400000, magic: 0x2008004124102200, shift: 59, offset: 4992 },
  MagicEntry { mask: 0x000A102040000000, magic: 0x010A82020100C800, shift: 59, offset: 5024 },
  MagicEntry { mask: 0x0014224000000000, magic: 0x80800202420A0200, shift: 59, offset: 5056 },
  MagicEntry { mask: 0x0028440200000000, magic: 0x0002040410020208, shift: 59, offset: 5088 },
  MagicEntry { mask: 0x0050080402000000, magic: 0x00001190204804C2, shift: 59, offset: 5120 },
  MagicEntry { mask: 0x0020100804020000, magic: 0x0020102008812040, shift: 59, offset: 5152 },
  MagicEntry { mask: 0x0040201008040200, magic: 0x09C0041800450920, shift: 58, offset: 5184 },
];
pub const BISHOP_TABLE_SIZE: usize = 5248;
