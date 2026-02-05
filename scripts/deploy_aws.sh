#!/bin/bash
# AWS Deployment Script for Markowitz Portfolio Optimizer
# Prerequisites: AWS CLI configured, Docker installed

set -e

# Configuration
AWS_REGION="${AWS_REGION:-eu-west-1}"
AWS_ACCOUNT_ID="${AWS_ACCOUNT_ID:-$(aws sts get-caller-identity --query Account --output text)}"
ECR_REPO_NAME="markowitz-shiny"
IMAGE_TAG="${IMAGE_TAG:-latest}"

echo "=== Markowitz Portfolio Optimizer - AWS Deployment ==="
echo "Region: $AWS_REGION"
echo "Account: $AWS_ACCOUNT_ID"
echo ""

# Step 1: Create ECR repository if it doesn't exist
echo "[1/5] Creating ECR repository..."
aws ecr describe-repositories --repository-names $ECR_REPO_NAME --region $AWS_REGION 2>/dev/null || \
    aws ecr create-repository --repository-name $ECR_REPO_NAME --region $AWS_REGION

# Step 2: Login to ECR
echo "[2/5] Logging into ECR..."
aws ecr get-login-password --region $AWS_REGION | \
    docker login --username AWS --password-stdin $AWS_ACCOUNT_ID.dkr.ecr.$AWS_REGION.amazonaws.com

# Step 3: Build Docker image
echo "[3/5] Building Docker image..."
docker build -t $ECR_REPO_NAME:$IMAGE_TAG .

# Step 4: Tag image for ECR
echo "[4/5] Tagging image..."
docker tag $ECR_REPO_NAME:$IMAGE_TAG \
    $AWS_ACCOUNT_ID.dkr.ecr.$AWS_REGION.amazonaws.com/$ECR_REPO_NAME:$IMAGE_TAG

# Step 5: Push to ECR
echo "[5/5] Pushing to ECR..."
docker push $AWS_ACCOUNT_ID.dkr.ecr.$AWS_REGION.amazonaws.com/$ECR_REPO_NAME:$IMAGE_TAG

echo ""
echo "=== Deployment Complete ==="
echo "Image: $AWS_ACCOUNT_ID.dkr.ecr.$AWS_REGION.amazonaws.com/$ECR_REPO_NAME:$IMAGE_TAG"
echo ""
echo "Next steps:"
echo "  1. Create an ECS cluster or EC2 instance"
echo "  2. Run the container with:"
echo "     docker run -d -p 3838:3838 $AWS_ACCOUNT_ID.dkr.ecr.$AWS_REGION.amazonaws.com/$ECR_REPO_NAME:$IMAGE_TAG"
echo ""
echo "For EC2 deployment with docker-compose:"
echo "  1. SSH to your EC2 instance"
echo "  2. Install Docker and docker-compose"
echo "  3. Copy docker-compose.yml to the instance"
echo "  4. Update image in docker-compose.yml to use ECR image"
echo "  5. Run: docker-compose up -d"
