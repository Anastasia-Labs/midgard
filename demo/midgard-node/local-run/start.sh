#!/bin/bash

# Midgard Local Run Start Script
# This script starts the Midgard node along with Prometheus and Grafana
# All services run with the same metrics and dashboards as the Docker setup

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get the absolute path of the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
NODE_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Configuration
PROMETHEUS_PORT=9090
GRAFANA_PORT=3001
MIDGARD_API_PORT=3000
MIDGARD_METRICS_PORT=9464
NODE_EXPORTER_PORT=9100
LOKI_PORT=3100
PROMTAIL_PORT=9080

# Data directories
DATA_DIR="$SCRIPT_DIR/data"
PROMETHEUS_DATA="$DATA_DIR/prometheus-data"
GRAFANA_DATA="$DATA_DIR/grafana-data"
GRAFANA_LOGS="$DATA_DIR/grafana-logs"
GRAFANA_PLUGINS="$DATA_DIR/grafana-plugins"

# PID file to track running services
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

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to check if a port is in use (only checks for LISTEN state)
port_in_use() {
    # Check if something is actually listening on the port, not just connected to it
    lsof -i ":$1" -sTCP:LISTEN >/dev/null 2>&1
}

# Function to wait for a service to be ready
wait_for_service() {
    local url=$1
    local service_name=$2
    local max_attempts=30
    local attempt=0
    
    print_info "Waiting for $service_name to be ready..."
    while [ $attempt -lt $max_attempts ]; do
        if curl -s "$url" > /dev/null 2>&1; then
            print_success "$service_name is ready!"
            return 0
        fi
        attempt=$((attempt + 1))
        sleep 1
    done
    
    print_error "$service_name failed to start within $max_attempts seconds"
    return 1
}

# Function to create necessary directories
create_directories() {
    print_info "Creating data directories..."
    mkdir -p "$PROMETHEUS_DATA"
    mkdir -p "$GRAFANA_DATA"
    mkdir -p "$GRAFANA_LOGS"
    mkdir -p "$GRAFANA_PLUGINS"
    
    # Set permissions
    chmod -R 777 "$DATA_DIR"
    print_success "Data directories created"
}

# Function to check prerequisites
check_prerequisites() {
    print_info "Checking prerequisites..."
    
    local missing_deps=()
    
    if ! command_exists "prometheus"; then
        missing_deps+=("prometheus")
    fi
    
    if ! command_exists "grafana-server"; then
        missing_deps+=("grafana")
    fi
    
    if ! command_exists "node"; then
        missing_deps+=("node")
    fi
    
    if ! command_exists "pnpm"; then
        missing_deps+=("pnpm")
    fi
    
    if [ ${#missing_deps[@]} -ne 0 ]; then
        print_error "Missing required dependencies: ${missing_deps[*]}"
        print_info "Please install the missing dependencies:"
        print_info "  - Prometheus: https://prometheus.io/download/"
        print_info "  - Grafana: https://grafana.com/grafana/download"
        print_info "  - Node.js: https://nodejs.org/"
        print_info "  - pnpm: npm install -g pnpm"
        exit 1
    fi
    
    print_success "All prerequisites are installed"
}

# Function to check if services are already running
check_running_services() {
    local has_conflict=false
    
    if port_in_use $PROMETHEUS_PORT; then
        print_warning "Port $PROMETHEUS_PORT (Prometheus) is already in use"
        has_conflict=true
    fi
    
    if port_in_use $GRAFANA_PORT; then
        print_warning "Port $GRAFANA_PORT (Grafana) is already in use"
        has_conflict=true
    fi
    
    if port_in_use $MIDGARD_API_PORT; then
        print_warning "Port $MIDGARD_API_PORT (Midgard API) is already in use"
        has_conflict=true
    fi
    
    if port_in_use $MIDGARD_METRICS_PORT; then
        print_warning "Port $MIDGARD_METRICS_PORT (Midgard Metrics) is already in use"
        has_conflict=true
    fi
    
    if [ "$has_conflict" = true ]; then
        print_error "Some ports are already in use. Please stop conflicting services or run: ./stop.sh"
        exit 1
    fi
}

# Function to start Node Exporter
start_node_exporter() {
    if ! command_exists "node_exporter"; then
        print_warning "Node Exporter not installed. System metrics (CPU, Memory) will not be available."
        print_info "Install with: sudo apt install prometheus-node-exporter"
        print_info "Or download from: https://github.com/prometheus/node_exporter/releases"
        return
    fi
    
    print_info "Starting Node Exporter on port $NODE_EXPORTER_PORT..."
    
    node_exporter \
        --web.listen-address=":$NODE_EXPORTER_PORT" \
        > "$DATA_DIR/node_exporter.log" 2>&1 &
    
    local pid=$!
    echo "node_exporter:$pid" >> "$PID_FILE"
    
    print_success "Node Exporter started (PID: $pid)"
    print_info "Node Exporter metrics: http://localhost:$NODE_EXPORTER_PORT/metrics"
}

# Function to start Loki
start_loki() {
    if ! command_exists "loki"; then
        print_warning "Loki not installed. Log aggregation in Grafana will not be available."
        print_info "Install from: https://github.com/grafana/loki/releases"
        print_info "Download and move to: /usr/local/bin/loki"
        return
    fi
    
    print_info "Starting Loki on port $LOKI_PORT..."
    
    loki \
        -config.file="$SCRIPT_DIR/loki-config.yaml" \
        > "$DATA_DIR/loki.log" 2>&1 &
    
    local pid=$!
    echo "loki:$pid" >> "$PID_FILE"
    
    print_success "Loki started (PID: $pid)"
    print_info "Loki API: http://localhost:$LOKI_PORT"
}

# Function to start Promtail
start_promtail() {
    if ! command_exists "promtail"; then
        print_warning "Promtail not installed. Logs will not be shipped to Loki."
        print_info "Install from: https://github.com/grafana/loki/releases"
        print_info "Download and move to: /usr/local/bin/promtail"
        return
    fi
    
    print_info "Starting Promtail on port $PROMTAIL_PORT..."
    
    # Run Promtail from script directory (so relative paths in config work)
    cd "$SCRIPT_DIR"
    promtail \
        -config.file="./promtail-config.yaml" \
        > "$DATA_DIR/promtail.log" 2>&1 &
    
    local pid=$!
    echo "promtail:$pid" >> "$PID_FILE"
    
    print_success "Promtail started (PID: $pid)"
    print_info "Promtail: http://localhost:$PROMTAIL_PORT"
}

# Function to start Prometheus
start_prometheus() {
    print_info "Starting Prometheus on port $PROMETHEUS_PORT..."
    
    prometheus \
        --config.file="$SCRIPT_DIR/prometheus.yml" \
        --storage.tsdb.path="$PROMETHEUS_DATA" \
        --web.enable-remote-write-receiver \
        --enable-feature=exemplar-storage \
        --enable-feature=native-histograms \
        --web.listen-address="0.0.0.0:$PROMETHEUS_PORT" \
        > "$DATA_DIR/prometheus.log" 2>&1 &
    
    local pid=$!
    echo "prometheus:$pid" >> "$PID_FILE"
    
    print_success "Prometheus started (PID: $pid)"
    print_info "Prometheus UI: http://localhost:$PROMETHEUS_PORT"
}

# Function to start Grafana
start_grafana() {
    print_info "Starting Grafana on port $GRAFANA_PORT..."
    
    # Create Grafana config directory structure
    local grafana_provisioning="$SCRIPT_DIR/grafana-provisioning"
    mkdir -p "$grafana_provisioning/datasources"
    mkdir -p "$grafana_provisioning/dashboards"
    
    # Copy datasources configuration
    cp "$SCRIPT_DIR/datasources.yml" "$grafana_provisioning/datasources/"
    
    # Copy dashboard file to provisioning location
    cp "$SCRIPT_DIR/dashboard.json" "$grafana_provisioning/dashboards/"
    
    # Create dashboards provisioning config with absolute path
    local dashboard_path="$grafana_provisioning/dashboards/dashboard.json"
    sed "s|DASHBOARD_PATH_PLACEHOLDER|$dashboard_path|g" "$SCRIPT_DIR/dashboards.yml" > "$grafana_provisioning/dashboards/dashboards.yml"
    
    print_info "Dashboard provisioned at: $dashboard_path"
    
    # Start Grafana
    GF_PATHS_DATA="$GRAFANA_DATA" \
    GF_PATHS_LOGS="$GRAFANA_LOGS" \
    GF_PATHS_PLUGINS="$GRAFANA_PLUGINS" \
    GF_PATHS_PROVISIONING="$grafana_provisioning" \
    GF_SERVER_HTTP_PORT="$GRAFANA_PORT" \
    GF_AUTH_ANONYMOUS_ENABLED=true \
    GF_AUTH_ANONYMOUS_ORG_ROLE=Admin \
    GF_FEATURE_TOGGLES_ENABLE="traceqlEditor metricsSummary" \
    grafana-server \
        --homepath=/usr/share/grafana \
        > "$DATA_DIR/grafana.log" 2>&1 &
    
    local pid=$!
    echo "grafana:$pid" >> "$PID_FILE"
    
    print_success "Grafana started (PID: $pid)"
    print_info "Grafana UI: http://localhost:$GRAFANA_PORT"
}

# Function to start Midgard node
start_midgard() {
    print_info "Starting Midgard node..."
    
    # Install and build SDK
    local SDK_DIR="$(cd "$NODE_DIR/../midgard-sdk" && pwd)"
    print_info "Installing midgard-sdk dependencies..."
    cd "$SDK_DIR"
    pnpm install > /dev/null 2>&1
    
    print_info "Building midgard-sdk..."
    pnpm run repack > /dev/null 2>&1
    
    # Install node dependencies
    print_info "Installing midgard-node dependencies..."
    cd "$NODE_DIR"
    pnpm install > /dev/null 2>&1
    
    print_info "Starting Midgard node on port $MIDGARD_API_PORT (metrics on $MIDGARD_METRICS_PORT)..."
    
    pnpm run listen > "$DATA_DIR/midgard.log" 2>&1 &
    
    local pid=$!
    echo "midgard:$pid" >> "$PID_FILE"
    
    cd "$SCRIPT_DIR"
    
    print_success "Midgard node started (PID: $pid)"
    print_info "Midgard API: http://localhost:$MIDGARD_API_PORT"
    print_info "Midgard Metrics: http://localhost:$MIDGARD_METRICS_PORT/metrics"
}

# Function to display service status
show_status() {
    echo ""
    print_success "All services are starting up..."
    echo ""
    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}Service URLs:${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo -e "  ${YELLOW}Midgard API:${NC}        http://localhost:$MIDGARD_API_PORT"
    echo -e "  ${YELLOW}Midgard Metrics:${NC}    http://localhost:$MIDGARD_METRICS_PORT/metrics"
    if command_exists "node_exporter"; then
        echo -e "  ${YELLOW}Node Exporter:${NC}      http://localhost:$NODE_EXPORTER_PORT/metrics"
    fi
    if command_exists "loki"; then
        echo -e "  ${YELLOW}Loki:${NC}               http://localhost:$LOKI_PORT"
    fi
    echo -e "  ${YELLOW}Prometheus:${NC}         http://localhost:$PROMETHEUS_PORT"
    echo -e "  ${YELLOW}Grafana:${NC}            http://localhost:$GRAFANA_PORT"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "${GREEN}Logs:${NC}"
    if command_exists "node_exporter"; then
        echo -e "  ${YELLOW}Node Exporter:${NC}      $DATA_DIR/node_exporter.log"
    fi
    if command_exists "loki"; then
        echo -e "  ${YELLOW}Loki:${NC}               $DATA_DIR/loki.log"
        echo -e "  ${YELLOW}Promtail:${NC}           $DATA_DIR/promtail.log"
    fi
    echo -e "  ${YELLOW}Prometheus:${NC}         $DATA_DIR/prometheus.log"
    echo -e "  ${YELLOW}Grafana:${NC}            $DATA_DIR/grafana.log"
    echo -e "  ${YELLOW}Midgard:${NC}            $DATA_DIR/midgard.log"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo ""
    if ! command_exists "node_exporter"; then
        print_warning "Node Exporter not installed - CPU/Memory dashboards will show 'No data'"
        print_info "Install: sudo apt install prometheus-node-exporter"
        print_info "Or see: INSTALL.md for installation instructions"
        echo ""
    fi
    if ! command_exists "loki"; then
        print_warning "Loki not installed - Logs in Grafana will not be available"
        print_info "See INSTALL.md for installation instructions"
        echo ""
    fi
    echo -e "${YELLOW}To stop all services, run:${NC} ./stop.sh"
    echo -e "${YELLOW}To view logs:${NC} tail -f $DATA_DIR/<service>.log"
    echo ""
}

# Cleanup function for graceful shutdown
cleanup() {
    print_warning "Received interrupt signal, stopping services..."
    "$SCRIPT_DIR/stop.sh"
    exit 0
}

# Main execution
main() {
    print_info "Starting Midgard Local Run..."
    echo ""
    
    # Set up trap for graceful shutdown
    trap cleanup INT TERM
    
    # Check prerequisites
    check_prerequisites
    
    # Check for port conflicts
    check_running_services
    
    # Create directories
    create_directories
    
    # Clear old PID file
    rm -f "$PID_FILE"
    
    # Start services
    start_node_exporter
    sleep 1
    
    start_loki
    sleep 2
    
    start_promtail
    sleep 1
    
    start_prometheus
    sleep 2
    
    start_grafana
    sleep 2
    
    start_midgard
    sleep 3
    
    # Wait for services to be ready
    if command_exists "node_exporter"; then
        wait_for_service "http://localhost:$NODE_EXPORTER_PORT/metrics" "Node Exporter"
    fi
    if command_exists "loki"; then
        wait_for_service "http://localhost:$LOKI_PORT/ready" "Loki"
    fi
    wait_for_service "http://localhost:$PROMETHEUS_PORT" "Prometheus"
    wait_for_service "http://localhost:$GRAFANA_PORT/api/health" "Grafana"
    wait_for_service "http://localhost:$MIDGARD_METRICS_PORT/metrics" "Midgard Node"
    
    # Show status
    show_status
    
    print_success "Midgard Local Run is running!"
    print_info "Press Ctrl+C to stop all services"
    
    # Keep script running
    wait
}

# Run main function
main

