#! /bin/bash

# Watch out for the workflow id - here 24790528
# Also need t set the appropriate sfSpHelpers
http_response=$(curl -s -o response.txt -w "%{http_code}"  \
              -X POST \
              -H "Accept: application/vnd.github+json" \
              -H "Authorization:token $GITHUB_PAT" \
              https://api.github.com/repos/cgauvi/sfSpHelpers/actions/workflows/24790528/dispatches \
              -d '{"ref":"master"}'
              )

# Inspect reponse
if [ $http_response != "204" ]; then
  echo "Error with http response"
  cat response.txt >&2
  exit 1
else
  echo "Successfully pinged github api"
fi

