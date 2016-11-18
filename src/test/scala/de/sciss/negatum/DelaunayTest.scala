package de.sciss.negatum

import de.sciss.negatum.Delaunay.Vector2

object DelaunayTest extends App {
  val even = Vector(
    Vector2(238.396f, 521.4281f),
    Vector2(161.6244f, 299.19452f),
    Vector2(224.25385f, 167.87468f),
    Vector2(832.36578f, 22.412703f),
    Vector2(1297.0359f, 10.290872f),
    Vector2(1299.0562f, 256.7681f),
    Vector2(1313.1982f, 717.39764f),
    Vector2(1317.2389f, 818.4129f),
    Vector2(458.60925f, 834.57538f),
    Vector2(684.88336f, 826.49414f),
    Vector2(905.09668f, 826.49414f),
    Vector2(1111.1677f, 820.43323f),
    Vector2(711.1474f, 145.65134f),
    Vector2(1119.249f, 472.94077f),
    Vector2(296.98483f, 733.56018f),
    Vector2(1305.1169f, 483.04227f),
    Vector2(1080.8633f, 10.290871f),
    Vector2(628.31488f, 52.717278f),
    Vector2(739.43152f, 737.60071f),
    Vector2(929.34027f, 479.00168f),
    Vector2(428.30466f, 119.38736f),
    Vector2(1485.2186f, 493.1438f),
    Vector2(1483.1171f, 817.96606f),
    Vector2(1559.1577f, 487.08289f)
  )

  val odd = Vector(
    Vector2(123.23861f, 190.09804f),
    Vector2(327.2894f, 147.67163f),
    Vector2(535.38086f, 91.103073f),
    Vector2(731.35046f, 28.473616f),
    Vector2(953.58405f, 22.412703f),
    Vector2(1183.8988f, 16.351786f),
    Vector2(1307.1375f, 121.40765f),
    Vector2(1305.1171f, 375.96609f),
    Vector2(272.74118f, 630.5246f),
    Vector2(339.41125f, 840.63635f),
    Vector2(565.68542f, 830.53473f),
    Vector2(791.95953f, 826.49414f),
    Vector2(327.2894f, 521.4281f),
    Vector2(735.39099f, 644.66675f),
    Vector2(1036.4165f, 476.98138f),
    Vector2(195.96959f, 406.27069f),
    Vector2(1010.1525f, 826.49414f),
    Vector2(1220.2642f, 822.45355f),
    Vector2(1309.1576f, 606.28088f),
    Vector2(1214.2034f, 464.85956f),
    Vector2(711.1474f, 250.7072f),
    Vector2(1405.2389f, 818.4129f),
    Vector2(1403.2186f, 493.1438f),
    Vector2(1559.1171f, 817.96606f)
  )

  val select   = odd // ++ even // ++ odd
  val selectS  = select.map { case Vector2(x, y) => Vector2(x * 0.5f, y * 0.5f) }

  Delaunay.mkProcessing(selectS)
}
