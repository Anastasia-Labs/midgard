#!/bin/bash

# Midgard Local Run Stop Script
# This script stops all services started by start.sh

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get the absolute path of the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# PID file
PID_FILE="$SCRIPT_DIR/.monitoring.pids"

# Function to print colored messages
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to kill a process and its children
kill_process_tree() {
    local pid=$1
    local service_name=$2
    
    if ps -p $pid > /dev/null 2>&1; then
        print_info "Stopping $service_name (PID: $pid)..."
        
        # Get all child processes
        local children=$(pgrep -P $pid 2>/dev/null || true)
        
        # Kill the main process
        kill $pid 2>/dev/null || true
        
        # Kill child processes
        if [ -n "$children" ]; then
            kill $children 2>/dev/null || true
        fi
        
        # Wait for process to terminate (max 5 seconds)
        local count=0
        while ps -p $pid > /dev/null 2>&1 && [ $count -lt 5 ]; do
            sleep 1
            count=$((count + 1))
        done
        
        # Force kill if still running
        if ps -p $pid > /dev/null 2>&1; then
            print_warning "Force killing $service_name (PID: $pid)..."
            kill -9 $pid 2>/dev/null || true
            if [ -n "$children" ]; then
                kill -9 $children 2>/dev/null || true
            fi
        fi
        
        print_success "$service_name stopped"
    else
        print_info "$service_name (PID: $pid) is not running"
    fi
}

# Function to stop all services
stop_services() {
    if [ ! -f "$PID_FILE" ]; then
        print_warning "No PID file found. Services may not be running."
        
        # Try to find and kill processes by name
        print_info "Searching for running services..."
        
        local found_any=false
        
        # Kill prometheus (try multiple patterns)
        local prom_pids=$(pgrep -f "prometheus.*local-run" || true)
        if [ -n "$prom_pids" ]; then
            print_info "Found Prometheus process(es): $prom_pids"
            echo "$prom_pids" | xargs kill 2>/dev/null || true
            sleep 1
            # Force kill if still running
            local still_running=$(ps -p $prom_pids 2>/dev/null | tail -n +2 || true)
            if [ -n "$still_running" ]; then
                echo "$prom_pids" | xargs kill -9 2>/dev/null || true
            fi
            print_success "Prometheus stopped"
            found_any=true
        fi
        
        # Kill grafana
        local grafana_pids=$(pgrep -f "grafana-server" || true)
        if [ -n "$grafana_pids" ]; then
            print_info "Found Grafana process(es): $grafana_pids"
            echo "$grafana_pids" | xargs kill 2>/dev/null || true
            sleep 1
            # Force kill if still running
            local still_running=$(ps -p $grafana_pids 2>/dev/null | tail -n +2 || true)
            if [ -n "$still_running" ]; then
                echo "$grafana_pids" | xargs kill -9 2>/dev/null || true
            fi
            print_success "Grafana stopped"
            found_any=true
        fi
        
        # Kill midgard node
        local midgard_pids=$(pgrep -f "pnpm.*listen" || true)
        if [ -n "$midgard_pids" ]; then
            print_info "Found Midgard Node process(es): $midgard_pids"
            echo "$midgard_pids" | xargs kill 2>/dev/null || true
            sleep 1
            # Force kill if still running
            local still_running=$(ps -p $midgard_pids 2>/dev/null | tail -n +2 || true)
            if [ -n "$still_running" ]; then
                echo "$midgard_pids" | xargs kill -9 2>/dev/null || true
            fi
            print_success "Midgard Node stopped"
            found_any=true
        fi
        
        # Kill node_exporter
        local node_exp_pids=$(pgrep -f "node_exporter" || true)
        if [ -n "$node_exp_pids" ]; then
            print_info "Found Node Exporter process(es): $node_exp_pids"
            echo "$node_exp_pids" | xargs kill 2>/dev/null || true
            sleep 1
            # Force kill if still running
            local still_running=$(ps -p $node_exp_pids 2>/dev/null | tail -n +2 || true)
            if [ -n "$still_running" ]; then
                echo "$node_exp_pids" | xargs kill -9 2>/dev/null || true
            fi
            print_success "Node Exporter stopped"
            found_any=true
        fi
        
        # Kill loki
        local loki_pids=$(pgrep -f "loki.*-config.file" || true)
        if [ -n "$loki_pids" ]; then
            print_info "Found Loki process(es): $loki_pids"
            echo "$loki_pids" | xargs kill 2>/dev/null || true
            sleep 1
            # Force kill if still running
            local still_running=$(ps -p $loki_pids 2>/dev/null | tail -n +2 || true)
            if [ -n "$still_running" ]; then
                echo "$loki_pids" | xargs kill -9 2>/dev/null || true
            fi
            print_success "Loki stopped"
            found_any=true
        fi
        
        # Kill promtail
        local promtail_pids=$(pgrep -f "promtail.*-config.file" || true)
        if [ -n "$promtail_pids" ]; then
            print_info "Found Promtail process(es): $promtail_pids"
            echo "$promtail_pids" | xargs kill 2>/dev/null || true
            sleep 1
            # Force kill if still running
            local still_running=$(ps -p $promtail_pids 2>/dev/null | tail -n +2 || true)
            if [ -n "$still_running" ]; then
                echo "$promtail_pids" | xargs kill -9 2>/dev/null || true
            fi
            print_success "Promtail stopped"
            found_any=true
        fi
        
        if [ "$found_any" = false ]; then
            print_info "No running services found"
        fi
        
        return
    fi
    
    print_info "Stopping Midgard services..."
    echo ""
    
    # Read PID file and stop each service
    while IFS=: read -r service pid; do
        if [ -n "$service" ] && [ -n "$pid" ]; then
            kill_process_tree "$pid" "$service"
        fi
    done < "$PID_FILE"
    
    # Remove PID file
    rm -f "$PID_FILE"
    
    echo ""
    print_success "All services stopped"
}

# Function to clean up provisioning directory
cleanup_provisioning() {
    local grafana_provisioning="$SCRIPT_DIR/grafana-provisioning"
    if [ -d "$grafana_provisioning" ]; then
        print_info "Cleaning up Grafana provisioning directory..."
        rm -rf "$grafana_provisioning"
    fi
}

# Main execution
main() {
    print_info "Stopping Midgard Local Run..."
    echo ""
    
    stop_services
    cleanup_provisioning
    
    echo ""
    print_success "Midgard Local Run stopped successfully"
    echo ""
}

# Run main function
main

