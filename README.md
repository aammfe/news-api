# news-api

  
News apis uses <https://newsapi.org/> as News Source. it has following endpoints,


## Top-headlines

 Returns Top Headlinesit, it has flowing queryParams

| name | values | default | required |
|---|---|---|---|
| pageNumber | >0 | 1 | no |
| pageSize | >0 | 10 | no |

example :`http://localhost:8080/top-headlines?&pageNumber=1&pageSize=1`


## Query

 Returns queried news feed, it has flowing query params

| name | values | default | required |
|---|---|---|---|
| queryBy | title \| description \| content | - | yes |
| query | keywords | - | yes |
| pageNumber | >0 | 1 | no |
| pageSize | >0 | 10 | no |
  

example :`http://localhost:8080/query?queryBy=title&query=cardano&pageNumber=1&pageSize=1`

  
  

## Getting Started

to Run `nix run . -- --newsApiKey api_key`

## Tips

  

- Run `nix flake update` to update all flake inputs.

- Run `nix run github:srid/nixci` to build _all_ outputs.

- Run `, fmt` in nix shell to autoformat the project. This uses [treefmt](https://github.com/numtide/treefmt).

- Run `, docs` to start Hoogle with packages in your cabal file.

- Run the application without installing: `nix run github:aammfe/news-api` (or `nix run .` from checkout)

- Common workflows
