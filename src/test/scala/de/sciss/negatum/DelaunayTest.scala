package de.sciss.negatum

import de.sciss.negatum.Delaunay.Vector2

object DelaunayTest extends App {
  val even = Vector(
    Vector2(238.396f, 521.4281f),
    Vector2(161.6244f, 299.19452f),
    Vector2(167.87468f, 224.25385f),
    Vector2(22.412703f, 832.36578f),
    Vector2(1297.0359f, 10.290872f),
    Vector2(1299.0562f, 256.7681f),
    Vector2(1313.1982f, 717.39764f),
    Vector2(818.4129f, 1317.2389f),
    Vector2(834.57538f, 458.60925f),
    Vector2(826.49414f, 684.88336f),
    Vector2(826.49414f, 905.09668f),
    Vector2(1111.1677f, 820.43323f),
    Vector2(145.65134f, 711.1474f),
    Vector2(1119.249f, 472.94077f),
    Vector2(296.98483f, 733.56018f),
    Vector2(1305.1169f, 483.04227f),
    Vector2(1080.8633f, 10.290871f),
    Vector2(52.717278f, 628.31488f),
    Vector2(737.60071f, 739.43152f),
    Vector2(929.34027f, 479.00168f),
    Vector2(119.38736f, 428.30466f),
    Vector2(493.1438f, 1485.2186f),
    Vector2(1483.1171f, 817.96606f),
    Vector2(1559.1577f, 487.08289f)
  )

  val odd = Vector(
    Vector2(123.23861f, 190.09804f),
    Vector2(327.2894f, 147.67163f),
    Vector2(91.103073f, 535.38086f),
    Vector2(731.35046f, 28.473616f),
    Vector2(953.58405f, 22.412703f),
    Vector2(16.351786f, 16.351786f),
    Vector2(121.40765f, 1307.1375f),
    Vector2(375.96609f, 1305.1171f),
    Vector2(630.5246f, 272.74118f),
    Vector2(339.41125f, 840.63635f),
    Vector2(565.68542f, 830.53473f),
    Vector2(791.95953f, 826.49414f),
    Vector2(521.4281f, 327.2894f),
    Vector2(735.39099f, 644.66675f),
    Vector2(476.98138f, 1036.4165f),
    Vector2(195.96959f, 406.27069f),
    Vector2(1010.1525f, 826.49414f),
    Vector2(822.45355f, 1220.2642f),
    Vector2(606.28088f, 1309.1576f),
    Vector2(464.85956f, 1214.2034f),
    Vector2(711.1474f, 250.7072f),
    Vector2(1405.2389f, 818.4129f),
    Vector2(1403.2186f, 493.1438f),
    Vector2(817.96606f, 1559.1171f)
  )

  Delaunay.mkProcessing(even ++ odd)
}
