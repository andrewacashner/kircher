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
a'2 g'2 e''2 f''2 
g'2 a'1 g'2 g'2. g'4 g'1 
a'1 b'2. a'4 g'2 a'2 
a'1 b'2. a'4 g'2 a'2 
a'1 b'2. a'4 g'2 a'2 
a'1 b'2. a'4 g'2 a'2 
}

MusicA = {
f''2 d''2 c''2 c''2 
e''2 f''1 e''2 d''2. d''4 e''1 
f''1 f''2. f''4 f''2 f''2 
f''1 f''2. f''4 f''2 f''2 
f''1 f''2. f''4 f''2 f''2 
f''1 f''2. f''4 f''2 f''2 
}

MusicT = {
c'2 b2 g2 a2 
c'2 c'1 c'2 b2. b4 c'1 
c'1 d'2. c'4 c'2 c'2 
c'1 d'2. c'4 c'2 c'2 
c'1 d'2. c'4 c'2 c'2 
c'1 d'2. c'4 c'2 c'2 
}

MusicB = {
a2 b2 c'2 f2 
c'2 f1 c'2 g2. g4 c'1 
f'1 b2. f'4 c'2 f2 
f'1 b2. f'4 c'2 f2 
f'1 b2. f'4 c'2 f2 
f'1 b2. f'4 c'2 f2 
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
              \MusicS
        }
        \new Lyrics \lyricsto "S" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "A" {
           \clef "treble"
              \time 4/2
              \MusicA
        }
        \new Lyrics \lyricsto "A" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "T" {
           \clef "treble_8"
              \time 4/2
              \MusicT
        }
        \new Lyrics \lyricsto "T" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "B" {
           \clef "bass"
              \time 4/2
              \MusicB
        }
        \new Lyrics \lyricsto "B" { \Lyrics }
      >>
    >>
  >>
}
