"""Neo4j server management utilities."""

import os
import subprocess
import time
import psutil
import logging
import shutil
from pathlib import Path
from typing import Optional, Tuple

logger = logging.getLogger(__name__)


class Neo4jServer:
    """Manage Neo4j server lifecycle."""
    
    def __init__(self, data_dir: Optional[str] = None):
        """Initialize Neo4j server manager.
        
        Args:
            data_dir: Path to Neo4j data directory. Defaults to ./neo4j-data
        """
        self.data_dir = Path(data_dir or os.path.join(os.getcwd(), 'neo4j-data'))
        self.config_file = self.data_dir / 'neo4j.conf'
        self.pid_file = self.data_dir / 'run' / 'neo4j.pid'
        self.log_dir = self.data_dir / 'logs'
        
    def _get_neo4j_command(self) -> str:
        """Get neo4j command path."""
        # First check if neo4j is available via shutil.which
        neo4j_path = shutil.which('neo4j')
        if neo4j_path:
            logger.debug(f"Found neo4j at: {neo4j_path}")
            return neo4j_path
        
        # If not in PATH, check common locations
        common_paths = [
            '/usr/bin/neo4j',
            '/usr/local/bin/neo4j',
            os.path.expanduser('~/.nix-profile/bin/neo4j'),
            '/nix/store/*/bin/neo4j'  # Nix store path pattern
        ]
        
        for path in common_paths:
            if '*' in path:
                # Handle glob patterns
                import glob
                matches = glob.glob(path)
                if matches:
                    logger.debug(f"Found neo4j at: {matches[0]}")
                    return matches[0]
            elif os.path.exists(path):
                logger.debug(f"Found neo4j at: {path}")
                return path
        
        raise RuntimeError(
            "Neo4j executable not found. Please ensure Neo4j is installed and in your PATH.\n"
            "You can install Neo4j via:\n"
            "  - Package manager (apt, brew, etc.)\n"
            "  - Nix: nix-env -iA nixpkgs.neo4j\n"
            "  - Download from https://neo4j.com/download/"
        )
    
    def _is_running(self) -> bool:
        """Check if Neo4j is running for this project."""
        try:
            for proc in psutil.process_iter(['pid', 'cmdline', 'name']):
                try:
                    # Check if this is actually a Java process running Neo4j
                    proc_name = proc.info.get('name', '').lower()
                    cmdline = proc.info.get('cmdline') or []
                    cmdline_str = ' '.join(cmdline)
                    
                    # Neo4j runs as a Java process with specific patterns
                    is_java = 'java' in proc_name
                    # Check for Neo4j server main class or neo4j in classpath
                    has_neo4j_class = any(
                        'neo4j' in arg.lower() and (
                            'CommunityEntryPoint' in arg or 
                            'EnterpriseEntryPoint' in arg or
                            '/neo4j/' in arg or
                            'neo4j-' in arg
                        ) for arg in cmdline
                    )
                    has_data_dir = str(self.data_dir) in cmdline_str
                    
                    if is_java and has_neo4j_class and has_data_dir:
                        logger.debug(f"Found Neo4j process: PID {proc.info['pid']}")
                        return True
                except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
                    continue
        except Exception as e:
            logger.warning(f"Error checking process list: {e}")
            
        # Fall back to checking pid file
        if self.pid_file.exists():
            try:
                with open(self.pid_file) as f:
                    pid = int(f.read().strip())
                # Check if the PID exists and is a Java process
                if psutil.pid_exists(pid):
                    try:
                        proc = psutil.Process(pid)
                        if 'java' in proc.name().lower():
                            return True
                    except (psutil.NoSuchProcess, psutil.AccessDenied):
                        pass
            except (ValueError, IOError):
                pass
        
        return False
    
    def _run_neo4j_command(self, *args) -> Tuple[int, str, str]:
        """Run neo4j command with arguments.
        
        Returns:
            Tuple of (returncode, stdout, stderr)
        """
        neo4j_cmd = self._get_neo4j_command()
        cmd = [neo4j_cmd] + list(args)
        
        # Set NEO4J_HOME environment variable
        env = os.environ.copy()
        env['NEO4J_HOME'] = str(self.data_dir.parent)
        env['NEO4J_CONF'] = str(self.data_dir)
        
        result = subprocess.run(cmd, capture_output=True, text=True, env=env)
        return result.returncode, result.stdout, result.stderr
    
    def start(self) -> bool:
        """Start Neo4j server.
        
        Returns:
            True if server started successfully, False otherwise
        """
        # Check if data directory exists
        if not self.data_dir.exists():
            logger.error(f"Neo4j data directory not found: {self.data_dir}")
            print(f"Error: Neo4j data directory not found: {self.data_dir}")
            print("Please ensure the Neo4j data directory is properly set up.")
            return False
        
        if self._is_running():
            logger.info("Neo4j is already running for this project")
            print("Neo4j is already running for this project")
            print("Access the browser at: http://localhost:7474")
            return True
        
        print("Starting Neo4j server...")
        try:
            returncode, stdout, stderr = self._run_neo4j_command('start')
        except Exception as e:
            logger.error(f"Failed to run neo4j command: {e}")
            print(f"Error: Failed to run neo4j command: {e}")
            return False
        
        if returncode != 0:
            logger.error(f"Failed to start Neo4j: {stderr}")
            print(f"Failed to start Neo4j: {stderr}")
            if "already running" in stderr.lower():
                print("Another Neo4j instance may be running on the same port.")
                print("Try stopping it first with: fortran-mapper neo4j stop")
            return False
        
        # Wait a bit for server to start
        print("Waiting for Neo4j to start...")
        for i in range(10):  # Wait up to 10 seconds
            time.sleep(1)
            if self._is_running():
                break
        
        # Check if server is running
        try:
            status_code, _, _ = self._run_neo4j_command('status')
            if status_code == 0:
                print("Neo4j started successfully!")
                print("Access the browser at: http://localhost:7474")
                print("Default credentials - username: neo4j, password: neo4j")
                return True
        except Exception:
            pass
        
        print(f"Neo4j may have started but status check failed.")
        print(f"Check logs at: {self.log_dir}/")
        print("Try: fortran-mapper neo4j status")
        return True  # Return True since start command succeeded
    
    def stop(self) -> bool:
        """Stop Neo4j server.
        
        Returns:
            True if server stopped successfully, False otherwise
        """
        if not self._is_running():
            print("Neo4j is not running for this project")
            # Clean up stale PID file if it exists
            if self.pid_file.exists():
                try:
                    self.pid_file.unlink()
                    logger.debug("Removed stale PID file")
                except Exception as e:
                    logger.warning(f"Could not remove PID file: {e}")
            return True
        
        print("Stopping Neo4j server...")
        try:
            returncode, stdout, stderr = self._run_neo4j_command('stop')
        except Exception as e:
            logger.error(f"Failed to run neo4j stop command: {e}")
            print(f"Error: Failed to run neo4j stop command: {e}")
            return False
        
        if returncode != 0:
            logger.error(f"Failed to stop Neo4j: {stderr}")
            print(f"Failed to stop Neo4j: {stderr}")
            return False
        
        # Wait a bit for the process to fully stop
        print("Waiting for Neo4j to stop...")
        for i in range(5):
            time.sleep(1)
            if not self._is_running():
                break
        
        # Clean up PID file if it still exists
        if self.pid_file.exists():
            try:
                self.pid_file.unlink()
                logger.debug("Removed PID file after stop")
            except Exception:
                pass
        
        print("Neo4j stopped successfully")
        return True
    
    def status(self, verbose: bool = False) -> bool:
        """Check Neo4j server status.
        
        Args:
            verbose: Show detailed process information
            
        Returns:
            True if server is running, False otherwise
        """
        if not self.config_file.exists():
            print(f"Neo4j config not found at {self.config_file}")
            print("Set up Neo4j data directory first")
            return False
        
        # Check for running process
        is_running = self._is_running()
        
        # If verbose, show what processes were checked
        if verbose:
            print("\nDebug: Checking for Neo4j processes...")
            found_any = False
            try:
                for proc in psutil.process_iter(['pid', 'cmdline', 'name']):
                    try:
                        cmdline = proc.info.get('cmdline') or []
                        cmdline_str = ' '.join(cmdline)
                        if 'neo4j' in cmdline_str.lower():
                            print(f"  Found process with 'neo4j': PID={proc.info['pid']}, Name={proc.info.get('name', 'unknown')}")
                            print(f"    Command: {cmdline_str[:100]}...")
                            found_any = True
                    except (psutil.NoSuchProcess, psutil.AccessDenied):
                        continue
                if not found_any:
                    print("  No processes with 'neo4j' in command line found")
            except Exception as e:
                print(f"  Error scanning processes: {e}")
            
            print(f"\nData directory: {self.data_dir}")
            print(f"PID file: {self.pid_file} (exists: {self.pid_file.exists()})")
            print()
        
        if is_running:
            print("Neo4j is running for this project")
            print("Access the browser at: http://localhost:7474")
            
            # Show detailed status
            try:
                returncode, stdout, stderr = self._run_neo4j_command('status')
                if stdout:
                    print(stdout)
            except Exception as e:
                logger.warning(f"Could not get detailed status: {e}")
            
            return True
        else:
            print("Neo4j is not running for this project")
            print("Start it with: fortran-mapper neo4j start")
            
            # Check if PID file exists but process is gone
            if self.pid_file.exists():
                print("\nNote: PID file exists but process is not running.")
                print("The server may have crashed or been killed.")
                print("Run 'fortran-mapper neo4j stop' to clean up.")
            
            return False
    
    def console(self) -> int:
        """Start Neo4j in console mode (foreground).
        
        Returns:
            Exit code from neo4j console command
        """
        print("Starting Neo4j in console mode (foreground)...")
        print("Press Ctrl+C to stop")
        
        neo4j_cmd = self._get_neo4j_command()
        
        # Set environment variables
        env = os.environ.copy()
        env['NEO4J_HOME'] = str(self.data_dir.parent)
        env['NEO4J_CONF'] = str(self.data_dir)
        
        # Run in foreground, allowing direct interaction
        try:
            result = subprocess.run([neo4j_cmd, 'console'], env=env)
            return result.returncode
        except KeyboardInterrupt:
            print("\nStopping Neo4j console...")
            return 0


def neo4j_server_command(action: str, data_dir: Optional[str] = None, verbose: bool = False) -> int:
    """Execute Neo4j server management command.
    
    Args:
        action: Action to perform (start, stop, status, console)
        data_dir: Optional Neo4j data directory path
        verbose: Enable verbose output for debugging
        
    Returns:
        Exit code (0 for success, non-zero for failure)
    """
    server = Neo4jServer(data_dir)
    
    try:
        if action == 'start':
            return 0 if server.start() else 1
        elif action == 'stop':
            return 0 if server.stop() else 1
        elif action == 'status':
            return 0 if server.status(verbose=verbose) else 1
        elif action == 'console':
            return server.console()
        else:
            print(f"Unknown action: {action}")
            return 1
    except Exception as e:
        logger.error(f"Error executing {action}: {e}")
        print(f"Error: {e}")
        return 1