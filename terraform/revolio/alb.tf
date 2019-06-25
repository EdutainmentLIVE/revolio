resource "aws_alb" "alb" {
  name = "${var.environment}-${var.app}"
  internal = true

  security_groups = var.security_groups
  subnets         = var.subnet_ids

  enable_deletion_protection = true
}

resource "aws_alb_target_group" "target_group_fargate" {
  name        = "${var.environment}-${var.app}-fargate"
  port        = 80
  protocol    = "HTTP"
  target_type = "ip"
  vpc_id      = var.vpc_id
  deregistration_delay = 10

  health_check {
    path = "/ping"
  }
}

resource "aws_alb_listener" "listener" {
  load_balancer_arn = aws_alb.alb.arn
  port              = "80"
  protocol          = "HTTP"

  default_action {
    target_group_arn = aws_alb_target_group.target_group_fargate.arn
    type             = "forward"
  }
}

