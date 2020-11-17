module Input.Ps150 where

import Aedifico
    (ArkConfig (..),
     Style     (..),
     Meter     (..),
     Mode      (..))

import Lectio
    (prepareText)

text = prepareText psalm config
    where
        config = ArkConfig {
            arkStyle = Simple, 
            arkMeter = TripleMinor, 
            arkMode  = Mode11
        }
        psalm = "Al-le-`lu-ia. Lau-`da-te `Do-mi-num in `sanc-tis `e-jus;\
\                lau-`da-te `e-um in fir-ma-`men-to vir-`tu-tis `e-ius.\
\                `Om-nis `spi-ri-tus `lau-det `Do-mi-num! Al-le-`lu-ia.\
\                `Glo-ri-a `Pat-ri et `Fi-li-o et Spi-`ri-tui `Sancto,\
\                `si-cut `e-rat in prin-`ci-pi-o et nunc et `sem-per et in\
\                `sae-cu-la sae-cu-`lo-rum. A-`men."
