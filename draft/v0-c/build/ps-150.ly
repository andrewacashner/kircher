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

MusicS = { f''2 e''2 c''1\sh d''1 e''2 f''4 f''4 e''4. e''8 f''2 f''4 g''2 g''4 f''1 g''1 e''1 f''2. f''4 e''2 f''2 f''2 g''1 f''2 e''1 f''1 r4 f'4 g'4 f'4 e'2 f'2 a'4 b'8\fl a'8 g'4 f'4 c''2.\sh d''4 e''2. e''4 e''1 a'4 a'4 e''4 f''8 f''8 e''4 f''4 b'2\fl a'1 b'2 r4 f''2 f''4 g''4. f''8 g''2 d''1. e''2 c''1.\sh c''2\sh d''1 a'4 a'4  \bar "|." }

MusicA = { d''2 b'2\fl a'1 a'1 c''2\sh d''4 d''4 c''4.\sh c''8\sh d''2 d''4 d''2 d''4 d''1 d''1 c''1\sh d''2. d''4 c''2\sh d''2 d''2 d''1 d''2 c''1\sh d''1 r4 d''4 d''4 d''4 c''2\sh d''2 d''4 d''8 d''8 d''4 d''4 a'2. a'4 b'2. b'4 c''1 c''4 c''4 c''4\sh d''8 d''8 c''4\sh d''4 d''2 d''1 d''2 r4 d''2 d''4 d''4. c''8\sh d''2 b'1.\fl b'2\fl a'1. a'2 a'1 c''4\sh d''4  \bar "|." }

MusicT = { a2 g2 e1 f1 a2 a4 a4 a4. a8 a2 a4 b2\fl b4\fl a1 b1 a1 a2. a4 a2 a2 a2 b1\fl a2 a1 a1 r4 a4 b4\fl a4 a2 a2 f4 g8 a8 b4\fl a4 e2. f4 g2. g4 a1 e'4 e'4 a4 a8 a8 a4 a4 g2 f1 g2 r4 a2 a4 b4. c'8 b2 f1. g2 e1. e2 f1 e4 f4  \bar "|." }

MusicB = { f2 g2 a1 d1 a2 f4 d4 a4. a8 d2 d4 b,2 g,4 d1 g1 a1 f2. d4 a2 d2 d2 g1 d2 a1 d1 r4 d4 g4 d4 a2 d2 d4 g8 f8 g4 d4 a2. f4 e2. e4 a1 a4 a4 a4 f8 d8 a4 d4 g2 d1 g2 r4 d2 d4 b,4.\fl a,8 g,2 b,1. g,2 a,1. a,2 d1 a4 d4  \bar "|." }

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
