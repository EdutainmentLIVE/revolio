checkout
eval "$( aws ecr get-login --no-include-email )"
url=$( aws ecr describe-repositories --output 'text' --query 'repositories[0].repositoryUri' --repository-names revolio )
tag=$url:commit-$SEMAPHORE_GIT_SHA
docker build --tag "$tag" .
docker push "$tag"
