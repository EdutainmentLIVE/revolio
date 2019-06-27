variable "cluster_name" {
  type = string
}

variable "image_tag" {
  type = string
}

variable "repo" {
  type = string
}

variable "vpc_id" {
  type = string
}

variable "region" {
  type = string
}

variable "subnet_ids" {
  type = list(string)
}

variable "security_groups" {
  type = list(string)
}

variable "app" {
  type = string
}

variable "environment" {
  type = string
}

variable "cloudwatch_log_group" {
  type = string
}

variable "desired_count" {
  type = number
}

variable "public_alb_arn" {
  type = string
}

variable "listener_host" {
  type = string
}