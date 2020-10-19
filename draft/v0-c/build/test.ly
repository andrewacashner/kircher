\version "2.19"
\include "ficta.ly"
\include "automatic-ties.ly"

Lyrics = \lyricmode {
  AL -- LE -- LU -- IA.
Lau -- da -- te Do -- mi -- num
in sanc -- tis e -- ius.
Lau -- da -- te e -- um 
in fir -- ma -- men -- to 
vir -- tu -- tis e -- ius.
Laud -- da -- te e -- um
in vir -- tu -- ti -- bus 
e -- ius;
lau -- da -- te e -- um 
se -- cun -- dum 
mul -- ti -- tu -- di -- nem
mag -- ni -- tu -- di -- nis 
e -- ius.
}

MusicS = { r2 a'1 a'2 a'1 a'1 a'4 g'8 f'8 e'4. e'8 f'1 r4 e'4 f'4 f'4 e'2 f'2 a'1 b'2.\fl b'4\fl a'2 b'2 e''2 d''1 e''2 c''1\sh d''1 a'2 b'1\fl a'2 a'1 a'1 g'4 f'2 e'4 c'1\sh d'1 f'1. f'2 g'1. f'2 g'1 a'2. a'4 f'2 g'4 g'4 f'2 g'2 a'2 g'1 f'2 g'4 g'4 f'4. e'8 f'1 d'1. e'2 c'1.\sh c'2\sh d'1 a'4 a'4  \bar "|." }

MusicA = { r2 c''1\sh d''2 c''1\sh d''1 d''4 d''8 d''8 c''4.\sh c''8\sh d''1 r4 c''4\sh d''4 d''4 c''2\sh d''2 c''1\sh d''2. d''4 d''2 d''2 b'2\fl a'1 b'2\fl a'1 a'1 d'2 d'1 d'2 c'1\sh d'1 e'4 d'2 b4\fl a1 a1 d'1. d'2 d'1. c'2\sh d'1 c'2.\sh d'4 d'2 d'4 d'4 d'2 d'2 d'2 d'1 d'2 d'4 e'4 d'4. c'8\sh d'1 b1.\fl b2\fl a1. a2 a1 d'4 c'4  \bar "|." }

MusicT = { r2 e'1 f'2 e'1 f'1 f'4 b8\fl a8 a4. a8 a1 r4 a4 a4 a4 a2 a2 f1 f2. g4 f2 g2 g2 f1 e2 e1 f1 f2 g1 f2 e1 f1 b4\fl a2 g4 e1 f1 a1. a2 b1. c'2 b1 e2. f4 a2 b4\fl b4\fl a2 b2 f'2 b1\fl a2 b4\fl b4\fl a4. a8 a1 f1. g2 e1. e2 f1 f4 e4  \bar "|." }

MusicB = { r2 a1 d2 a1 d1 d4 g8 d8 a4. a8 d1 r4 a4 f4 d4 a2 d2 f1 b2. g4 d2 g2 e2 f1 g2 a1 d1 d2 g1 a2 a1 d1 e4 f2 g4 a1 d1 d1. d2 b,1.\fl a,2 g,1 a,2. d4 d2 b,4 g,4 d2 g2 d2 g1 d2 g4 e4 f4. a8 d1 b,1. g,2 a,1. a,2 d1 d4 a4  \bar "|." }

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
