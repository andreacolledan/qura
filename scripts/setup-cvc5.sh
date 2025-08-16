#!/usr/bin/env bash

CVC5_VERSION=1.3.0

if [[ "$RUNNER_OS" == "Linux" ]]; then
  PLATFORM="Linux-x86_64"
elif [[ "$RUNNER_OS" == "macOS" ]]; then
  PLATFORM="macOS-arm64"  # or macOS-x86_64
elif [[ "$RUNNER_OS" == "Windows" ]]; then
  PLATFORM="Win64-x86_64"
else
  echo "Unsupported OS: $RUNNER_OS"
  exit 1
fi

echo "Setting up cvc5 for platform: $PLATFORM"

curl -L -o cvc5.zip https://github.com/cvc5/cvc5/releases/download/cvc5-${CVC5_VERSION}/cvc5-${PLATFORM}-static.zip
unzip cvc5.zip

if [[ "$RUNNER_OS" == "Windows" ]]; then
  echo "Adding cvc5 to PATH..."
  echo "$(pwd)/cvc5-${PLATFORM}-static/bin" >> "$GITHUB_PATH"
else
  sudo mv cvc5-${PLATFORM}-static/bin/cvc5 /usr/local/bin/
  sudo chmod +x /usr/local/bin/cvc5
fi
