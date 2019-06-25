resource "aws_ecr_lifecycle_policy" "repo_policies" {
    repository = var.repo_name
    policy     = <<JSON
  {
    "rules": [
      {
        "rulePriority": 1,
        "description": "not-too-many-untagged-images",
        "selection": {
          "tagStatus": "untagged",
          "countType": "imageCountMoreThan",
          "countNumber": 10
        },
        "action": {
          "type": "expire"
        }
      },
      {
        "rulePriority": 2,
        "description": "not-too-many-tagged-images",
        "selection": {
          "tagStatus": "tagged",
          "tagPrefixList": [
            "commit-"
          ],
          "countType": "imageCountMoreThan",
          "countNumber": 10
        },
        "action": {
          "type": "expire"
        }
      }
    ]
  }
  JSON
}