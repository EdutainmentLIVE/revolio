checkout
cd "terraform" || exit
echo "AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID" > settings.env
echo "AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY" >> settings.env
./init
./tf get
./tf apply -auto-approve -target="module.$ENVIRONMENT" -var "$ENVIRONMENT={git_sha=\"$SEMAPHORE_GIT_SHA\"}"
