#!/bin/bash
# Mock aider script for testing

LOG_FILE="${AIDER_MOCK_LOG:-/tmp/aider_mock.log}"

# Log the command-line arguments
echo "ARGS: $@" > "$LOG_FILE"

# Log stdin if it's not a tty
if [ ! -t 0 ]; then
  cat >> "$LOG_FILE"
fi

# Simulate some output
echo "Mock aider response."
exit 0
