# GHCi Session Management

## Session Lifecycle

1. **Startup**: 90s timeout, auto-detect library target
2. **Healthy**: All commands execute normally
3. **Degraded**: Warnings or slow responses
4. **Corrupted**: Post-timeout, requires restart

## Health Monitoring

The MCP automatically monitors session health and reports it in tool responses.

### Health States

- **healthy**: Session is operating normally
- **degraded**: Session is slow or showing warnings (rare)
- **corrupted**: Session has timed out and needs restart

### Checking Health

Session health is automatically tracked. After a timeout, the session is marked as corrupted and will auto-restart on the next tool call.

## Recovery Strategies

### Auto-recovery (Default)

When a session times out:
1. Session is marked as `corrupted`
2. GHCi process is killed (SIGTERM)
3. Next MCP tool call automatically restarts the session
4. Session health is reset to `healthy`

No manual intervention required.

### Manual Restart

If you need to manually restart a session (rare):
- The session will restart automatically on next tool call
- All persistent imports are re-applied after restart

## Timeout Behavior

### Default Timeouts

- Normal commands: 30 seconds
- Session startup: 90 seconds

### What Happens on Timeout

1. Command execution is aborted
2. Session is marked as corrupted
3. GHCi process is killed
4. Error is returned to user
5. Next tool call triggers auto-recovery

## Forbidden Patterns

These patterns break the MCP's sentinel-based communication protocol:

### Never Use in .ghci Files

- `:set +m` (multiline mode) - breaks sentinel detection
- `:set prompt "..."` - overrides MCP's sentinel prompt
- `:set prompt-cont "..."` - breaks multi-line command handling

### Never Use in ghci_batch

- Commands containing `:set +m`
- Commands containing `:set prompt`

These will be rejected with an error before execution.

## Best Practices

### 1. Let Auto-Recovery Work

Don't try to manually restart sessions after timeouts. The MCP handles this automatically.

### 2. Use Appropriate Timeouts

For long-running computations, consider:
- Breaking into smaller steps
- Using `ghci_eval` with explicit timeout parameter
- Testing with smaller inputs first

### 3. Monitor for Patterns

If you see frequent timeouts:
- Check for infinite loops in your code
- Verify QuickCheck properties aren't generating huge test cases
- Consider using `resize` in Arbitrary instances

### 4. Trust the Health System

The MCP's health monitoring is automatic and reliable. If a session is corrupted, it will be restarted. You don't need to check or manage this manually.

## Troubleshooting

### Session Feels Stuck

- Next tool call will detect corruption and auto-restart
- No manual intervention needed

### Repeated Timeouts

- Check your code for infinite loops
- Verify QuickCheck generators are bounded
- Consider smaller test inputs

### Unexpected Behavior After Timeout

- Session auto-restarts on next call
- All state is reset to clean
- Persistent imports are re-applied

## Technical Details

### Sentinel Protocol

The MCP uses a sentinel-based protocol to detect command completion:
- Unique sentinel string: `<<<GHCi-DONE-7f3a2b>>>`
- Sent after each command
- Used to detect when GHCi output is complete

This is why `:set prompt` and `:set +m` are forbidden - they break this protocol.

### Health Tracking

Session health is tracked internally:
- `lastExecutedCommand`: Last command sent to GHCi
- `bufferSize`: Size of pending output buffer
- `sessionHealth`: Current health state

### Auto-Kill on Timeout

When a command times out:
1. Timeout timer fires
2. Session health set to `corrupted`
3. GHCi process killed with SIGTERM
4. Pending promises rejected with timeout error

This prevents zombie processes and ensures clean recovery.
