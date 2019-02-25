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
f'2 e'2 c''2 d''2 
e''1 f''2 f''2 e''1. e''2 f''1 
g'4 f'2 e'4 c''1 d''1 
a'1 b'2 a'2 g'1 f'1 
e''1 f''2. f''4 e''2 f''2 
e'4 d'8 e'8 c''4 d''4 
 \bar "|." }

MusicA = {
d''2 b'2 a'2 a'2 
c''1 d''2 d''2 c''1. c''2 d''1 
e'4 d''2 b'4 a'1 a'1 
d''1 d''2 d''2 d''1 d''1 
c''1 d''2. d''4 c''2 d''2 
b'4 a'8 b'8 a'4 a'4 
 \bar "|." }

MusicT = {
a'2 g'2 e'2 f'2 
a'1 a'2 a'2 a'1. a'2 a'1 
b4 a2 g4 e1 f1 
f1 g2 a2 b1 a1 
a'1 a'2. a'4 a'2 a'2 
g'4 f'8 e'8 e'4 f'4 
 \bar "|." }

MusicB = {
f2 g2 a2 d2 
a1 f2 d2 a1. a2 d1 
e4 f2 g4 a1 d1 
d1 g2 f2 g1 d1 
a1 f2. d4 a2 d2 
e4 f8 g8 a4 d4 
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
              \key c\major
              \MusicS
        }
        \new Lyrics \lyricsto "S" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "A" {
           \clef "treble"
              \time 4/2
              \key c\major
              \MusicA
        }
        \new Lyrics \lyricsto "A" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "T" {
           \clef "treble_8"
              \time 4/2
              \key c\major
              \MusicT
        }
        \new Lyrics \lyricsto "T" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "B" {
           \clef "bass"
              \time 4/2
              \key c\major
              \MusicB
        }
        \new Lyrics \lyricsto "B" { \Lyrics }
      >>
    >>
  >>
}
