#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"jane-street-build-server"
  [ oasis_exe "jane-street-build-server" ~dest:"jane-street-build-server"
  ]
