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
r2 a1 bes2 a1 bes1 
d4 c8 bes8 a4. a8 bes1 
r4 bes4 c4 c4 bes2 c2 
d4 es8 d8 c4 bes4 
r4 bes4 c4 c4 bes2 c2 
bes1 c2 bes2 a1 bes1 
 \bar "|." }

MusicA = {
r2 fis1 g2 fis1 g1 
g4 g8 g8 fis4. fis8 g1 
r4 g4 g4 g4 g2 g2 
g4 g8 g8 g4 g4 
r4 g4 g4 g4 g2 g2 
g1 g2 g2 g1 g1 
 \bar "|." }

MusicT = {
r2 d1 d2 d1 d1 
bes4 es8 d8 d4. d8 d1 
r4 d4 es4 es4 d2 es2 
bes4 c8 d8 es4 d4 
r4 d4 es4 es4 d2 es2 
d1 es2 d2 d1 d1 
 \bar "|." }

MusicB = {
r2 d1 g2 d1 g1 
g4 c8 g8 d4. d8 g1 
r4 g4 es4 c4 g2 c2 
g4 c8 bes8 c4 g4 
r4 g4 es4 c4 g2 c2 
g1 c2 g2 d1 g1 
 \bar "|." }

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
