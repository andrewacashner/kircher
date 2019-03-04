\version "2.19"

Lyrics = \lyricmode {
  AL -- LE -- LU -- IA.
Lau -- da -- te Do -- mi -- num
in sanc -- tis e -- ius.
Lau -- da -- te e -- um 
in fir -- ma -- men -- to 
vir -- tu -- tis e -- ius.
}

MusicS = { d''2  ees''2  d''2  d''2  d''4  c''8  b'8  a'4.  a'8  b'1  r4  d''4  ees''4  d''4  c''2  b'2  a'1  g'2.  a'4  fis'2  g'2  d''1  ees''2.  ees''4  d''2  d''2  d''2  ees''4  ees''4  d''2  d''2   \bar "|." }

MusicA = { g'2  g'2  fis'2  g'2  b'4  a'8  g'8  g'4.  fis'8  g'1  r4  g'4  g'4  g'4  g'2  g'2  ees'1  d'2.  ees'4  d'2  d'2  fis'1  g'2.  g'4  g'2  g'2  fis'2  g'4  g'4  g'2  g'2   \bar "|." }

MusicT = { b2  c'2  a2  b2  g'4  ees'8  d'8  ees'4.  d'8  d'1  r4  b4  c'4  d'4  ees'2  d'2  c'1  b2.  a4  a2  b2  b1  b2.  c'4  b2  c'2  b2  b4  c'4  b2  c'2   \bar "|." }

MusicB = { g2  c2  d2  g,2  g,4  a,8  b,8  c4.  d8  g,1  r4  g4  c4  b,4  c2  g,2  a,1  b,2.  c4  d2  g,2  b,1  e2.  c4  g2  c2  b,2  e4  c4  g2  c2   \bar "|." }

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
