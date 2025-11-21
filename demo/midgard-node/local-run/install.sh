#!/bin/bash

# Midgard Local Monitoring - Automated Installation Script (Linux)
# Installs all required dependencies for local monitoring without Docker

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

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

# Detect Linux distribution
detect_distro() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        DISTRO=$ID
        VERSION=$VERSION_ID
    else
        DISTRO="unknown"
    fi
}

# Install Node.js and pnpm
install_node_pnpm() {
    print_info "Checking Node.js and pnpm..."
    
    if command_exists "node"; then
        NODE_VERSION=$(node --version | cut -d'v' -f2 | cut -d'.' -f1)
        if [ "$NODE_VERSION" -ge 18 ]; then
            print_success "✓ Node.js $(node --version) already installed"
        else
            print_warning "Node.js version too old (need v18+)"
            print_info "Please upgrade manually from: https://nodejs.org/"
            print_info "Or use NodeSource: curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -"
            exit 1
        fi
    else
        print_error "Node.js not found. Please install Node.js v18+ from https://nodejs.org/"
        exit 1
    fi
    
    if command_exists "pnpm"; then
        print_success "✓ pnpm already installed"
    else
        print_info "Installing pnpm..."
        npm install -g pnpm
        print_success "✓ pnpm installed"
    fi
}

# Install Prometheus
install_prometheus() {
    if command_exists "prometheus"; then
        print_success "✓ Prometheus already installed"
        return
    fi
    
    print_info "Installing Prometheus..."
    
    if [ "$DISTRO" = "ubuntu" ] || [ "$DISTRO" = "debian" ]; then
        sudo apt update
        sudo apt install -y prometheus
    elif [ "$DISTRO" = "arch" ] || [ "$DISTRO" = "manjaro" ]; then
        sudo pacman -S --noconfirm prometheus
    elif [ "$DISTRO" = "fedora" ] || [ "$DISTRO" = "rhel" ] || [ "$DISTRO" = "centos" ]; then
        sudo dnf install -y prometheus
    else
        # Manual installation for other distros
        print_info "Using manual installation for your distribution..."
        PROM_VERSION="2.45.0"
        wget https://github.com/prometheus/prometheus/releases/download/v${PROM_VERSION}/prometheus-${PROM_VERSION}.linux-amd64.tar.gz
        tar xvf prometheus-${PROM_VERSION}.linux-amd64.tar.gz
        sudo mv prometheus-${PROM_VERSION}.linux-amd64/prometheus /usr/local/bin/
        sudo chmod +x /usr/local/bin/prometheus
        rm -rf prometheus-${PROM_VERSION}.linux-amd64*
    fi
    
    print_success "✓ Prometheus installed"
}

# Install Grafana
install_grafana() {
    if command_exists "grafana-server"; then
        print_success "✓ Grafana already installed"
        return
    fi
    
    print_info "Installing Grafana..."
    
    if [ "$DISTRO" = "ubuntu" ] || [ "$DISTRO" = "debian" ]; then
        sudo apt-get install -y software-properties-common
        sudo add-apt-repository -y "deb https://packages.grafana.com/oss/deb stable main"
        wget -q -O - https://packages.grafana.com/gpg.key | sudo apt-key add -
        sudo apt-get update
        sudo apt-get install -y grafana
    elif [ "$DISTRO" = "arch" ] || [ "$DISTRO" = "manjaro" ]; then
        sudo pacman -S --noconfirm grafana
    elif [ "$DISTRO" = "fedora" ] || [ "$DISTRO" = "rhel" ] || [ "$DISTRO" = "centos" ]; then
        sudo dnf install -y grafana
    else
        print_error "Automated Grafana installation not supported for $DISTRO"
        print_info "Please install manually from: https://grafana.com/grafana/download"
        print_info "Or download the binary and place it in /usr/local/bin/"
        exit 1
    fi
    
    print_success "✓ Grafana installed"
}

# Install Node Exporter
install_node_exporter() {
    if command_exists "node_exporter"; then
        print_success "✓ Node Exporter already installed"
        return
    fi
    
    print_info "Installing Node Exporter..."
    
    if [ "$DISTRO" = "ubuntu" ] || [ "$DISTRO" = "debian" ]; then
        sudo apt install -y prometheus-node-exporter
    elif [ "$DISTRO" = "arch" ] || [ "$DISTRO" = "manjaro" ]; then
        sudo pacman -S --noconfirm prometheus-node-exporter
    elif [ "$DISTRO" = "fedora" ] || [ "$DISTRO" = "rhel" ] || [ "$DISTRO" = "centos" ]; then
        sudo dnf install -y golang-github-prometheus-node-exporter
    else
        # Manual installation
        print_info "Using manual installation for your distribution..."
        NE_VERSION="1.6.1"
        wget https://github.com/prometheus/node_exporter/releases/download/v${NE_VERSION}/node_exporter-${NE_VERSION}.linux-amd64.tar.gz
        tar xvf node_exporter-${NE_VERSION}.linux-amd64.tar.gz
        sudo mv node_exporter-${NE_VERSION}.linux-amd64/node_exporter /usr/local/bin/
        sudo chmod +x /usr/local/bin/node_exporter
        rm -rf node_exporter-${NE_VERSION}.linux-amd64*
    fi
    
    print_success "✓ Node Exporter installed"
}

# Install Loki
install_loki() {
    if command_exists "loki"; then
        print_success "✓ Loki already installed"
        return
    fi
    
    print_info "Installing Loki..."
    
    LOKI_VERSION="3.4.1"
    wget https://github.com/grafana/loki/releases/download/v${LOKI_VERSION}/loki-linux-amd64.zip
    unzip -o loki-linux-amd64.zip
    sudo mv loki-linux-amd64 /usr/local/bin/loki
    sudo chmod +x /usr/local/bin/loki
    rm loki-linux-amd64.zip
    
    print_success "✓ Loki installed"
}

# Install Promtail
install_promtail() {
    if command_exists "promtail"; then
        print_success "✓ Promtail already installed"
        return
    fi
    
    print_info "Installing Promtail..."
    
    PROMTAIL_VERSION="2.9.0"
    wget https://github.com/grafana/loki/releases/download/v${PROMTAIL_VERSION}/promtail-linux-amd64.zip
    unzip -o promtail-linux-amd64.zip
    sudo mv promtail-linux-amd64 /usr/local/bin/promtail
    sudo chmod +x /usr/local/bin/promtail
    rm promtail-linux-amd64.zip
    
    print_success "✓ Promtail installed"
}

# Verify installation
verify_installation() {
    print_info "Verifying installation..."
    echo ""
    
    local all_installed=true
    
    if command_exists "node"; then
        print_success "✓ Node.js: $(node --version)"
    else
        print_error "✗ Node.js not found"
        all_installed=false
    fi
    
    if command_exists "pnpm"; then
        print_success "✓ pnpm: $(pnpm --version)"
    else
        print_error "✗ pnpm not found"
        all_installed=false
    fi
    
    if command_exists "prometheus"; then
        print_success "✓ Prometheus: $(prometheus --version 2>&1 | head -1)"
    else
        print_error "✗ Prometheus not found"
        all_installed=false
    fi
    
    if command_exists "grafana-server"; then
        print_success "✓ Grafana: $(grafana-server -v 2>&1 | head -1)"
    else
        print_error "✗ Grafana not found"
        all_installed=false
    fi
    
    if command_exists "node_exporter"; then
        print_success "✓ Node Exporter: installed"
    else
        print_error "✗ Node Exporter not found"
        all_installed=false
    fi
    
    if command_exists "loki"; then
        print_success "✓ Loki: $(loki --version 2>&1 | head -1)"
    else
        print_error "✗ Loki not found"
        all_installed=false
    fi
    
    if command_exists "promtail"; then
        print_success "✓ Promtail: $(promtail --version 2>&1 | head -1)"
    else
        print_error "✗ Promtail not found"
        all_installed=false
    fi
    
    echo ""
    
    if [ "$all_installed" = true ]; then
        print_success "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        print_success "   All dependencies installed successfully!"
        print_success "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo ""
        print_info "Next steps:"
        echo "  1. Run: ./start.sh"
        echo "  2. Open Grafana: http://localhost:3001"
        echo ""
    else
        print_error "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        print_error "   Some dependencies failed to install"
        print_error "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo ""
        print_info "Please check the errors above and:"
        echo "  - Install missing dependencies manually (see INSTALL.md)"
        echo "  - Or run: ./install.sh again"
        echo ""
        exit 1
    fi
}

# Check for required tools
check_requirements() {
    print_info "Checking system requirements..."
    
    local missing_tools=()
    
    if ! command_exists "wget"; then
        missing_tools+=("wget")
    fi
    
    if ! command_exists "unzip"; then
        missing_tools+=("unzip")
    fi
    
    if [ ${#missing_tools[@]} -ne 0 ]; then
        print_error "Missing required tools: ${missing_tools[*]}"
        echo ""
        print_info "Install them with:"
        
        if [ "$DISTRO" = "ubuntu" ] || [ "$DISTRO" = "debian" ]; then
            echo "  sudo apt install ${missing_tools[*]}"
        elif [ "$DISTRO" = "arch" ] || [ "$DISTRO" = "manjaro" ]; then
            echo "  sudo pacman -S ${missing_tools[*]}"
        elif [ "$DISTRO" = "fedora" ] || [ "$DISTRO" = "rhel" ] || [ "$DISTRO" = "centos" ]; then
            echo "  sudo dnf install ${missing_tools[*]}"
        else
            echo "  Use your package manager to install: ${missing_tools[*]}"
        fi
        echo ""
        exit 1
    fi
    
    print_success "✓ System requirements met"
}

# Main installation process
main() {
    echo ""
    print_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    print_info "   Midgard Local Monitoring - Automated Installation"
    print_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    
    # Handle --verify flag
    if [ "$1" = "--verify" ]; then
        verify_installation
        exit 0
    fi
    
    # Detect distribution
    detect_distro
    print_info "Detected Linux distribution: $DISTRO"
    echo ""
    
    # Check requirements
    check_requirements
    echo ""
    
    # Install each component
    install_node_pnpm
    install_prometheus
    install_grafana
    install_node_exporter
    install_loki
    install_promtail
    
    # Verify everything is installed
    echo ""
    verify_installation
}

# Run main function
main "$@"
