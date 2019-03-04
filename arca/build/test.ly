\version "2.19"

Lyrics = \lyricmode {
  AL -- LE -- LU -- IA.
Lau -- da -- te Do -- mi -- num
in sanc -- tis e -- ius.
Lau -- da -- te e -- um 
in fir -- ma -- men -- to 
vir -- tu -- tis e -- ius.
}

MusicS = { e''1  e''2  d''1.  e''1.  d''2  e''2  d''2  d''2.  d''4  d''2  e''2  f''2  f''2  e''1  f''2  d''2  e''2  e''2  d''1  e''2  g'2  a'2  g'2  f'1  e'2  g'2  a'2  g'2  f'1  e'2   \bar "|." }

MusicA = { c''1  c''2  b'1.  c''1.  b'2  c''2  b'2  a'2.  a'4  b'2  c''2  c''2  c''2  c''1  c''2  b'2  c''2  c''2  b'1  c''2  c''2  c''2  c''2  c''1  c''2  c''2  c''2  c''2  c''1  c''2   \bar "|." }

MusicT = { g1  g2  g1.  g1.  g2  g2  g2  f2.  f4  g2  g2  a2  a2  g1  a2  g2  g2  g2  g1  g2  e2  f2  g2  a1  g2  e2  f2  g2  a1  g2   \bar "|." }

MusicB = { c'1  c'2  g1.  c1.  g2  c2  g2  d2.  d4  g2  c'2  a2  f2  c1  f2  g2  e2  c2  g1  c2  c'2  f2  e2  f1  c2  c'2  f2  e2  f1  c2   \bar "|." }

\score {
  <<
    \new ChoirStaff
    <<
      \new Staff
      <<
        \new Voice = "S" {
           \clef "treble"
              \time 3/2
              \key c\major
              \MusicS
        }
        \new Lyrics \lyricsto "S" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "A" {
           \clef "treble"
              \time 3/2
              \key c\major
              \MusicA
        }
        \new Lyrics \lyricsto "A" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "T" {
           \clef "treble_8"
              \time 3/2
              \key c\major
              \MusicT
        }
        \new Lyrics \lyricsto "T" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "B" {
           \clef "bass"
              \time 3/2
              \key c\major
              \MusicB
        }
        \new Lyrics \lyricsto "B" { \Lyrics }
      >>
    >>
  >>
}
