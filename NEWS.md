
# portrpaths 1.0.0

## Breaking Changes

- Shared config functionality totally removed
  [#34](https://github.com/mstr3336/portrpaths/issues/34),
  [#33](https://github.com/mstr3336/portrpaths/issues/33), 
  for the following reasons:
  - It added unnecessarycomplexity
  - Made path maintenence too rigid / difficult to collaborate on
  - Was inflexible in how data paths could be set up, and very opinionated
  - The functionality I intended it for is better provided by
    [pathlibr](https://github.com/mstr3336/pathlibr), or user-side use of 
    `file.path(my_portrpath$root, "path", "to", "file")`.
