/**
  * Retlinx Message Processor
  *
  *
  * @author Redi Tibuludji
  */
package com.retlinx.msg.format.charset

trait Charset {
  val a2e: Array[Byte] = Array[Byte](
      0.toByte,   1.toByte,   2.toByte,   3.toByte,  55.toByte,  45.toByte,  46.toByte,  47.toByte,
     22.toByte,   5.toByte,  37.toByte,  11.toByte,  12.toByte,  13.toByte,  14.toByte,  15.toByte,
     16.toByte,  17.toByte,  18.toByte,  19.toByte,  60.toByte,  61.toByte,  50.toByte,  38.toByte,
     24.toByte,  25.toByte,  63.toByte,  39.toByte,  28.toByte,  29.toByte,  30.toByte,  31.toByte,
     64.toByte,  79.toByte, 127.toByte, 123.toByte,  91.toByte, 108.toByte,  80.toByte, 125.toByte,
     77.toByte,  93.toByte,  92.toByte,  78.toByte, 107.toByte,  96.toByte,  75.toByte,  97.toByte,
    240.toByte, 241.toByte, 242.toByte, 243.toByte, 244.toByte, 245.toByte, 246.toByte, 247.toByte,
    248.toByte, 249.toByte, 122.toByte,  94.toByte,  76.toByte, 126.toByte, 110.toByte, 111.toByte,
    124.toByte, 193.toByte, 194.toByte, 195.toByte, 196.toByte, 197.toByte, 198.toByte, 199.toByte,
    200.toByte, 201.toByte, 209.toByte, 210.toByte, 211.toByte, 212.toByte, 213.toByte, 214.toByte,
    215.toByte, 216.toByte, 217.toByte, 226.toByte, 227.toByte, 228.toByte, 229.toByte, 230.toByte,
    231.toByte, 232.toByte, 233.toByte,  74.toByte, 224.toByte,  90.toByte,  95.toByte, 109.toByte,
    121.toByte, 129.toByte, 130.toByte, 131.toByte, 132.toByte, 133.toByte, 134.toByte, 135.toByte,
    136.toByte, 137.toByte, 145.toByte, 146.toByte, 147.toByte, 148.toByte, 149.toByte, 150.toByte,
    151.toByte, 152.toByte, 153.toByte, 162.toByte, 163.toByte, 164.toByte, 165.toByte, 166.toByte,
    167.toByte, 168.toByte, 169.toByte, 192.toByte, 106.toByte, 208.toByte, 161.toByte,   7.toByte,
     32.toByte,  33.toByte,  34.toByte,  35.toByte,  36.toByte,  21.toByte,   6.toByte,  23.toByte,
     40.toByte,  41.toByte,  42.toByte,  43.toByte,  44.toByte,   9.toByte,  10.toByte,  27.toByte,
     48.toByte,  49.toByte,  26.toByte,  51.toByte,  52.toByte,  53.toByte,  54.toByte,   8.toByte,
     56.toByte,  57.toByte,  58.toByte,  59.toByte,   4.toByte,  20.toByte,  62.toByte, 225.toByte,
     65.toByte,  66.toByte,  67.toByte,  68.toByte,  69.toByte,  70.toByte,  71.toByte,  72.toByte,
     73.toByte,  81.toByte,  82.toByte,  83.toByte,  84.toByte,  85.toByte,  86.toByte,  87.toByte,
     88.toByte,  89.toByte,  98.toByte,  99.toByte, 100.toByte, 101.toByte, 102.toByte, 103.toByte,
    104.toByte, 105.toByte, 112.toByte, 113.toByte, 114.toByte, 115.toByte, 116.toByte, 117.toByte,
    118.toByte, 119.toByte, 120.toByte, 128.toByte, 138.toByte, 139.toByte, 140.toByte, 141.toByte,
    142.toByte, 143.toByte, 144.toByte, 154.toByte, 155.toByte, 156.toByte, 157.toByte, 158.toByte,
    159.toByte, 160.toByte, 170.toByte, 171.toByte, 172.toByte, 173.toByte, 174.toByte, 175.toByte,
    176.toByte, 177.toByte, 178.toByte, 179.toByte, 180.toByte, 181.toByte, 182.toByte, 183.toByte,
    184.toByte, 185.toByte, 186.toByte, 187.toByte, 188.toByte, 189.toByte, 190.toByte, 191.toByte,
    202.toByte, 203.toByte, 204.toByte, 205.toByte, 206.toByte, 207.toByte, 218.toByte, 219.toByte,
    220.toByte, 221.toByte, 222.toByte, 223.toByte, 234.toByte, 235.toByte, 236.toByte, 237.toByte,
    238.toByte, 239.toByte, 250.toByte, 251.toByte, 252.toByte, 253.toByte, 254.toByte, 255.toByte
  )

  val e2a: Array[Byte] = Array[Byte](
      0.toByte,   1.toByte,   2.toByte,   3.toByte, 156.toByte,   9.toByte, 134.toByte, 127.toByte,
    151.toByte, 141.toByte, 142.toByte,  11.toByte,  12.toByte,  13.toByte,  14.toByte,  15.toByte,
     16.toByte,  17.toByte,  18.toByte,  19.toByte, 157.toByte, 133.toByte,   8.toByte, 135.toByte,
     24.toByte,  25.toByte, 146.toByte, 143.toByte,  28.toByte,  29.toByte,  30.toByte,  31.toByte,
    128.toByte, 129.toByte, 130.toByte, 131.toByte, 132.toByte,  10.toByte,  23.toByte,  27.toByte,
    136.toByte, 137.toByte, 138.toByte, 139.toByte, 140.toByte,   5.toByte,   6.toByte,   7.toByte,
    144.toByte, 145.toByte,  22.toByte, 147.toByte, 148.toByte, 149.toByte, 150.toByte,   4.toByte,
    152.toByte, 153.toByte, 154.toByte, 155.toByte,  20.toByte,  21.toByte, 158.toByte,  26.toByte,
     32.toByte, 160.toByte, 161.toByte, 162.toByte, 163.toByte, 164.toByte, 165.toByte, 166.toByte,
    167.toByte, 168.toByte,  91.toByte,  46.toByte,  60.toByte,  40.toByte,  43.toByte,  33.toByte,
     38.toByte, 169.toByte, 170.toByte, 171.toByte, 172.toByte, 173.toByte, 174.toByte, 175.toByte,
    176.toByte, 177.toByte,  93.toByte,  36.toByte,  42.toByte,  41.toByte,  59.toByte,  94.toByte,
     45.toByte,  47.toByte, 178.toByte, 179.toByte, 180.toByte, 181.toByte, 182.toByte, 183.toByte,
    184.toByte, 185.toByte, 124.toByte,  44.toByte,  37.toByte,  95.toByte,  62.toByte,  63.toByte,
    186.toByte, 187.toByte, 188.toByte, 189.toByte, 190.toByte, 191.toByte, 192.toByte, 193.toByte,
    194.toByte,  96.toByte,  58.toByte,  35.toByte,  64.toByte,  39.toByte,  61.toByte,  34.toByte,
    195.toByte,  97.toByte,  98.toByte,  99.toByte, 100.toByte, 101.toByte, 102.toByte, 103.toByte,
    104.toByte, 105.toByte, 196.toByte, 197.toByte, 198.toByte, 199.toByte, 200.toByte, 201.toByte,
    202.toByte, 106.toByte, 107.toByte, 108.toByte, 109.toByte, 110.toByte, 111.toByte, 112.toByte,
    113.toByte, 114.toByte, 203.toByte, 204.toByte, 205.toByte, 206.toByte, 207.toByte, 208.toByte,
    209.toByte, 126.toByte, 115.toByte, 116.toByte, 117.toByte, 118.toByte, 119.toByte, 120.toByte,
    121.toByte, 122.toByte, 210.toByte, 211.toByte, 212.toByte, 213.toByte, 214.toByte, 215.toByte,
    216.toByte, 217.toByte, 218.toByte, 219.toByte, 220.toByte, 221.toByte, 222.toByte, 223.toByte,
    224.toByte, 225.toByte, 226.toByte, 227.toByte, 228.toByte, 229.toByte, 230.toByte, 231.toByte,
    123.toByte,  65.toByte,  66.toByte,  67.toByte,  68.toByte,  69.toByte,  70.toByte,  71.toByte,
     72.toByte,  73.toByte, 232.toByte, 233.toByte, 234.toByte, 235.toByte, 236.toByte, 237.toByte,
    125.toByte,  74.toByte,  75.toByte,  76.toByte,  77.toByte,  78.toByte,  79.toByte,  80.toByte,
     81.toByte,  82.toByte, 238.toByte, 239.toByte, 240.toByte, 241.toByte, 242.toByte, 243.toByte,
     92.toByte, 159.toByte,  83.toByte,  84.toByte,  85.toByte,  86.toByte,  87.toByte,  88.toByte,
     89.toByte,  90.toByte, 244.toByte, 245.toByte, 246.toByte, 247.toByte, 248.toByte, 249.toByte,
     48.toByte,  49.toByte,  50.toByte,  51.toByte,  52.toByte,  53.toByte,  54.toByte,  55.toByte,
     56.toByte,  57.toByte, 250.toByte, 251.toByte, 252.toByte, 253.toByte, 254.toByte, 255.toByte
  )
}
