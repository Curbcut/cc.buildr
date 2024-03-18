aws ecr get-login-password --region us-east-1 --profile Max | docker login --username AWS --password-stdin 805394637149.dkr.ecr.us-east-1.amazonaws.com
$imageID = docker images --filter=reference='toronto*' --quiet --all
docker tag $imageID registry.heroku.com/cc-805394637149/web
docker push 805394637149.dkr.ecr.us-east-1.amazonaws.com/curbcut-toronto:latest
