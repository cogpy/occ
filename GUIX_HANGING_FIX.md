# Guix Build Hanging Issue - Fixed

**Date:** 2025-10-22  
**Status:** ✅ FIXED

## Problem

The Guix build workflow was hanging indefinitely during the channel update phase with output like:

```
building CA certificate bundle...
listing Emacs sub-directories...
building fonts directory...
building directory of Info manuals...
building profile with 1 package...
hint: Consider installing the `glibc-locales' package and defining
`GUIX_LOCPATH', along these lines:
     guix install glibc-locales
     export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
See the "Application Setup" section in the manual, for more info.
Updating channel 'guix' from Git repository at 'https://git.savannah.gnu.org/git/guix.git'...
```

The process would hang at the "Updating channel..." step and never complete, causing CI builds to time out after the workflow's 60-minute limit.

## Root Cause

The issue occurred in the "Update Guix channels and profile" step of `.github/workflows/guix-build.yml`:

1. **Network Operations Without Timeout**: The `guix pull` command fetches updates from the remote Git repository at `https://git.savannah.gnu.org/git/guix.git`. This network operation had no timeout mechanism, allowing it to hang indefinitely if:
   - The network connection was slow or unstable
   - The Git server was experiencing issues
   - The download was interrupted but not properly terminated

2. **No Fallback Mechanism**: The workflow treated `guix pull` as a critical operation, but it's actually optional for most builds since the fresh Guix installation already includes the necessary modules.

3. **Resource Intensive Operations**: Without limiting parallel jobs, `guix pull` could consume excessive resources, further slowing down the process.

## Solution

Added timeout protection and graceful degradation to prevent indefinite hanging:

### Changes Made to `.github/workflows/guix-build.yml`

#### 1. Added Timeout to `glibc-locales` Installation (Line 49)
```bash
timeout 300 sudo -i guix install glibc-locales || echo "Warning: glibc-locales install timed out or failed (continuing anyway)"
```
- **Timeout**: 5 minutes (300 seconds)
- **Behavior**: Continues even if installation fails or times out

#### 2. Added Timeout to `guix pull` (Lines 56-60)
```bash
timeout 600 sudo -i bash -c "export GUIX_LOCPATH=/root/.guix-profile/lib/locale && guix pull --max-jobs=2 --fallback" || {
  echo "Warning: guix pull timed out or failed after 10 minutes"
  echo "Continuing with existing Guix installation - this is acceptable for most builds"
}
```
- **Timeout**: 10 minutes (600 seconds)
- **Additional flags**: 
  - `--max-jobs=2`: Limits parallel jobs to reduce resource consumption
  - `--fallback`: Uses fallback build methods if substitutes fail
- **Behavior**: Gracefully continues with existing Guix installation if timeout occurs

#### 3. Added Timeout to `guix package -u` (Lines 63-67)
```bash
timeout 300 sudo -i bash -c "export GUIX_LOCPATH=/root/.guix-profile/lib/locale && guix package -u" || {
  echo "Warning: guix package -u timed out or failed"
  echo "Continuing with existing packages"
}
```
- **Timeout**: 5 minutes (300 seconds)
- **Behavior**: Continues with existing packages if update fails

## Impact

These changes ensure that:

1. **No Indefinite Hanging**: All network operations have hard time limits
2. **Graceful Degradation**: The workflow continues even if updates fail
3. **Better Resource Management**: Limited parallel jobs prevent resource exhaustion
4. **Informative Output**: Clear messages explain what's happening and why
5. **Acceptable Builds**: Most builds work fine with the fresh Guix installation without requiring the latest updates

## Timeout Values

The timeout values were chosen based on:

- **glibc-locales (5 min)**: Small package, should install quickly
- **guix pull (10 min)**: Large operation downloading channel updates; 10 minutes is generous but prevents indefinite hangs
- **guix package -u (5 min)**: Package updates; typically quick or unnecessary

These timeouts can be adjusted if needed, but they should prevent the hanging issue while allowing sufficient time for normal operations.

## Testing

To verify the fix works:

1. The workflow should complete within the 60-minute timeout even if network is slow
2. Failed/timed-out updates should not prevent the build from proceeding
3. Clear warning messages should indicate when operations time out
4. The actual build steps should still execute even if updates fail

## Alternative Approaches Considered

1. **Skip `guix pull` entirely**: Would work, but updating ensures latest bug fixes
2. **Use substitutes only**: Faster, but may not always be available
3. **Cache Guix installation**: Complex to implement in GitHub Actions
4. **Longer timeouts**: Would still risk hanging, just for longer

The chosen approach balances reliability (won't hang) with functionality (tries to update when possible).

## Related Files

- `.github/workflows/guix-build.yml` - Main workflow file (modified)
- `GUIX_BUILD_FIXES.md` - Previous fixes documentation
- `GUIX_CI_MODULE_FIX.md` - Module availability fixes
- `.guix/README.md` - Guix usage documentation

## Future Improvements

Potential enhancements:

1. **Cache mechanism**: Cache Guix channels between runs
2. **Conditional updates**: Only update on schedule, not every build
3. **Parallel timeout strategy**: Try fast substitutes first, fall back to builds with timeout
4. **Network diagnostics**: Check connectivity before attempting updates

## Verification

Run the workflow and verify:

```bash
# The workflow should show output like:
Attempting guix pull with 10 minute timeout...
# ... either completes or shows:
Warning: guix pull timed out or failed after 10 minutes
Continuing with existing Guix installation - this is acceptable for most builds
```

The key success criterion is that the workflow **completes** rather than hanging indefinitely.

## Summary

✅ **Problem**: `guix pull` hanging indefinitely during channel updates  
✅ **Solution**: Added `timeout` command with 10-minute limit  
✅ **Impact**: Workflow can no longer hang indefinitely  
✅ **Trade-off**: May skip updates, but builds still work with fresh installation  
✅ **Result**: Reliable CI builds that complete within expected timeframes
