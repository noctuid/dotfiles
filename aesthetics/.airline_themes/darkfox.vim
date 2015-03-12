" based on default airline themes
let g:airline#themes#darkfox#palette = {}

function! airline#themes#darkfox#refresh()
  let g:airline#themes#darkfox#palette.accents = {
        \ 'red': airline#themes#get_highlight('Constant'),
        \ }

  let s:N1 = airline#themes#get_highlight2(['DbgCurrent', 'bg'], ['Folded', 'fg'], 'bold')
  let s:N2 = airline#themes#get_highlight('Folded')
  let s:N3 = airline#themes#get_highlight('NonText')

  let g:airline#themes#darkfox#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
  let s:Nmod = airline#themes#get_highlight('Comment')
  let g:airline#themes#darkfox#palette.normal_modified = {
        \ 'airline_c': s:Nmod
        \ }

  let s:I1 = airline#themes#get_highlight2(['DbgCurrent', 'bg'], ['String', 'fg'], 'bold')
  let s:I2 = airline#themes#get_highlight2(['String', 'fg'], ['Folded', 'bg'])
  let s:I3 = s:N3
  " kolor insert colors
  " consider using laederon instead
  let s:II1 = [ '#242322' , '#7eaefd' , 234 , 111 ]
  let s:II2 = [ '#75d7d8' , '#242322' , 80  , 234 ]
  let s:II3 = [ '#e2e2e2' , '#4a4a4a' , 254 , 238 ]

  let g:airline#themes#darkfox#palette.insert = airline#themes#generate_color_map(s:II1, s:II2, s:I3)
  let g:airline#themes#darkfox#palette.insert_modified = g:airline#themes#darkfox#palette.normal_modified

  let s:R1 = airline#themes#get_highlight2(['DbgCurrent', 'bg'], ['Comment', 'fg'], 'bold')
  let s:R2 = airline#themes#get_highlight2(['Comment', 'fg'], ['Folded', 'bg'])
  let s:R3 = s:N3
  let g:airline#themes#darkfox#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)
  let g:airline#themes#darkfox#palette.replace_modified = g:airline#themes#darkfox#palette.normal_modified

  let s:V1 = airline#themes#get_highlight2(['DbgCurrent', 'bg'], ['Identifier', 'fg'], 'bold')
  let s:V2 = airline#themes#get_highlight2(['Identifier', 'fg'], ['Folded', 'bg'])
  let s:V3 = s:N3

  " laederon visual mode colors
  let s:VV1 = [ '#1a1a18' , '#ab3e5d' , 232 , 161 ] " blackestgravel & raspberry
  let s:VV2 = [ '#000000' , '#908571' , 16 , 252 ] " coal & winterterrain
  let s:VV3 = [ '#ab3e5d' , '#8c7f77' , 161 , 245 ] " raspberry & wetcoldterrain

  let g:airline#themes#darkfox#palette.visual = airline#themes#generate_color_map(s:VV1, s:V2, s:V3)
  let g:airline#themes#darkfox#palette.visual_modified = g:airline#themes#darkfox#palette.normal_modified

  let s:IA = airline#themes#get_highlight('NonText')
  let g:airline#themes#darkfox#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
  let g:airline#themes#darkfox#palette.inactive_modified = {
        \ 'airline_c': s:Nmod
        \ }
endfunction

call airline#themes#darkfox#refresh()

