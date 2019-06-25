resource "aws_cloudwatch_log_group" "log" {
  name = var.cloudwatch_log_group
}

module "revolio_service_config" {
  source = "../itprotv-service-config/revolio/production_config_1.0"
}

resource "aws_ecs_task_definition" "task_def_fargate" {
  family                   = "${var.environment}-${var.app}-fargate"
  network_mode             = "awsvpc"
  cpu                      = 256
  memory                   = 512
  execution_role_arn       = aws_iam_role.ecsTaskExecutionRole.arn
  requires_compatibilities = ["FARGATE"]
  container_definitions    = <<EOF
[
{
  "name": "${var.app}",
  "image": "${var.repo}:${var.image_tag}",
  "essential": true,
  "memory": 64,
  "portMappings": [
    { "containerPort": 80 }
  ],
  "environment": [
    {
      "name": "STRATUS_TIME_CLIENT_ID",
      "value": "${module.revolio_service_config.stratus_time_client_id}"
    },
    {
      "name": "SLACK_SIGNING_SECRET",
      "value": "${module.revolio_service_config.slack_signing_secret}"
    },
    {
      "name": "STRATUS_TIME_BASE_URL",
      "value": "${module.revolio_service_config.stratus_time_base_url}"
    }
  ],
  "logConfiguration": {
    "logDriver": "awslogs",
    "options": {
      "awslogs-region": "${var.region}",
      "awslogs-group": "${aws_cloudwatch_log_group.log.name}",
      "awslogs-stream-prefix": "${var.app}"
    }
  }
}
]
EOF

}

resource "aws_ecs_service" "service_fargate" {
  name = "${var.environment}-${var.app}"
  task_definition = aws_ecs_task_definition.task_def_fargate.arn
  cluster = var.cluster_name
  desired_count = var.desired_count
  deployment_minimum_healthy_percent = 75
  launch_type = "FARGATE"

  network_configuration {
    subnets = var.subnet_ids
    security_groups = var.security_groups
    assign_public_ip = "true"
  }

  load_balancer {
    target_group_arn = aws_alb_target_group.target_group_fargate.arn
    container_name = "${var.app}"
    container_port = 80
  }
}

# https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html
resource "aws_iam_role" "ecsTaskExecutionRole" {
  name = "${var.app}-${var.environment}-ecs-exec"
  assume_role_policy = data.aws_iam_policy_document.assume_role_policy.json
}

data "aws_iam_policy_document" "assume_role_policy" {
  statement {
    actions = ["sts:AssumeRole"]

    principals {
      type = "Service"
      identifiers = ["ecs-tasks.amazonaws.com"]
    }
  }
}

# TODO: We should investigate if we can control the policy ourselves, so far we have been unsuccessful
resource "aws_iam_role_policy_attachment" "ecsTaskExecutionRole_policy" {
  role = aws_iam_role.ecsTaskExecutionRole.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

