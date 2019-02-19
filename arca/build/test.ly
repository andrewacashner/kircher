\version "2.19"

Lyrics = \lyricmode {
  AL -- LE -- LU -- IA.
Lau -- da -- te Do -- mi -- num
in sanc -- tis e -- ius.
Lau -- da -- te e -- um 
in fir -- ma -- men -- to 
vir -- tu -- tis e -- ius.
}

MusicS = {
r4 d'2 es'4 d'2 d'2 
g'4 c'8 d'8 es'4. es'8 d'1 
d'1 es'2 d'2 d'1 d'1 
bes'2 c'1 bes'2 a'1 bes'1 
a'4 bes'2 bes'4 a'1 bes'1 
r4 a'4 bes'4 bes'4 a'2 bes'2 
}

MusicA = {
r4 g'2 g'4 fis'2 g'2 
es'4 g'8 g'8 g'4. g'8 g'1 
g'1 g'2 g'2 fis'1 g'1 
g'2 g'1 g'2 g'1 g'1 
fis'4 g'2 g'4 fis'1 g'1 
r4 fis'4 g'4 g'4 fis'2 g'2 
}

MusicT = {
r4 bes2 c4 a2 bes2 
c4 es8 bes8 c4. c8 bes1 
bes1 c2 bes2 a1 bes1 
d2 es1 d2 d1 d1 
d4 d2 d4 d1 d1 
r4 d4 d4 d4 d2 d2 
}

MusicB = {
r4 g2 c4 d2 g2 
c4 c8 g8 c4. c8 g1 
g1 c2 d2 d1 g1 
g2 c1 g2 d1 g1 
d4 bes2 g4 d1 g1 
r4 d4 bes4 g4 d2 g2 
}

\score {
  <<
    \new ChoirStaff
    <<
      \new Staff
      <<
        \new Voice = "S" {
           \clef "treble"
              \time 4/2
              \key f\major
              \MusicS
        }
        \new Lyrics \lyricsto "S" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "A" {
           \clef "treble"
              \time 4/2
              \key f\major
              \MusicA
        }
        \new Lyrics \lyricsto "A" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "T" {
           \clef "treble_8"
              \time 4/2
              \key f\major
              \MusicT
        }
        \new Lyrics \lyricsto "T" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "B" {
           \clef "bass"
              \time 4/2
              \key f\major
              \MusicB
        }
        \new Lyrics \lyricsto "B" { \Lyrics }
      >>
    >>
  >>
}
