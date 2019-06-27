provider "aws" {
  version = "~> 2.0"
  region  = "us-east-1"
}

terraform {
  backend "s3" {
    bucket = "itprotv-terraform-state"
    key    = "revolio.tfstate"
    region = "us-east-1"
  }
}

data "terraform_remote_state" "genesis" {
  backend = "s3"
  config = {
    bucket = "itprotv-terraform-state"
    key    = "project-genesis.tfstate"
    region = "us-east-1"
  }
}

resource "aws_ecr_repository" "repo" {
  name = "revolio"
}

module "lifecycle-tag" {
  source = "./lifecycle-tag"
  repo_name = aws_ecr_repository.repo.name
}

module "production" {
  source = "./revolio"
  environment = "prod"
  app = "revolio"

  cloudwatch_log_group = "prod-revolio"
  listener_host = "aws.revolio.us"
  public_alb_arn = data.terraform_remote_state.genesis.outputs.production_public_alb_arn

  vpc_id = data.terraform_remote_state.genesis.outputs.vpc_id
  cluster_name = data.terraform_remote_state.genesis.outputs.production_cluster_name
  subnet_ids = data.terraform_remote_state.genesis.outputs.vpc_subnet_ids
  security_groups = data.terraform_remote_state.genesis.outputs.vpc_security_groups
  desired_count = 1

  repo = aws_ecr_repository.repo.repository_url
  image_tag = "commit-${var.production["git_sha"]}"
  region = "us-east-1"
}