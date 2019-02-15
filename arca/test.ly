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
r2 a1 bes2 a1 a1 
e4 d8 e8 e4. e8 e1 
e4 d2 e4 cis1 d1 
g4 f8 e8 cis4 d4 
a4 bes2 a4 g1 f1 
r4 f4 g4 f4 e2 f2 
}

MusicA = {
r2 d1 d2 cis1 d1 
cis4 bes8 cis8 bes4. bes8 cis1 
bes4 a2 bes4 a1 a1 
e4 d8 bes8 a4 a4 
d4 d2 d4 d1 d1 
r4 d4 d4 d4 d2 d2 
}

MusicT = {
r2 f1 g2 e1 f1 
a4 g8 a8 g4. g8 a1 
g4 f2 e4 e1 f1 
bes4 a8 g8 e4 f4 
f4 g2 a4 bes1 a1 
r4 a4 bes4 a4 a2 a2 
}

MusicB = {
r2 d1 g2 a1 d1 
a4 e8 a8 e4. e8 a1 
e4 f2 g4 a1 d1 
e4 f8 g8 a4 d4 
d4 g2 f4 g1 d1 
r4 d4 g4 d4 a2 d2 
}

\score {
  <<
    \new ChoirStaff
    <<
      \new Staff
      <<
        \new Voice = "S" {
           \clef "treble"
           \relative c'' {
              \time 4/2
              \MusicS
           }
        }
        \new Lyrics \lyricsto "S" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "A" {
           \clef "treble"
           \relative c'' {
              \time 4/2
              \MusicA
           }
        }
        \new Lyrics \lyricsto "A" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "T" {
           \clef "treble_8"
           \relative c' {
              \time 4/2
              \MusicT
           }
        }
        \new Lyrics \lyricsto "T" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "B" {
           \clef "bass"
           \relative c' {
              \time 4/2
              \MusicB
           }
        }
        \new Lyrics \lyricsto "B" { \Lyrics }
      >>
    >>
  >>
}
