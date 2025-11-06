# Installation Guide - Midgard Local Monitoring (Linux)

This guide covers installing all required dependencies for running Midgard with local monitoring on Linux.

## Quick Install (Automated)

```bash
./install.sh
```

This will automatically install all dependencies on your Linux system.

---

## Manual Installation

### 1. Prometheus

**Ubuntu/Debian:**
```bash
sudo apt update
sudo apt install prometheus
```

**Manual Installation:**
```bash
wget https://github.com/prometheus/prometheus/releases/download/v2.45.0/prometheus-2.45.0.linux-amd64.tar.gz
tar xvf prometheus-2.45.0.linux-amd64.tar.gz
sudo mv prometheus-2.45.0.linux-amd64/prometheus /usr/local/bin/
sudo chmod +x /usr/local/bin/prometheus
rm -rf prometheus-2.45.0.linux-amd64*
```

### 2. Grafana

**Ubuntu/Debian:**
```bash
sudo apt-get install -y software-properties-common
sudo add-apt-repository "deb https://packages.grafana.com/oss/deb stable main"
wget -q -O - https://packages.grafana.com/gpg.key | sudo apt-key add -
sudo apt-get update
sudo apt-get install grafana
```

**Arch Linux:**
```bash
sudo pacman -S grafana
```

**Other Distributions:** See https://grafana.com/grafana/download

### 3. Node Exporter (System Metrics)

**Ubuntu/Debian:**
```bash
sudo apt install prometheus-node-exporter
```

**Manual Installation:**
```bash
wget https://github.com/prometheus/node_exporter/releases/download/v1.6.1/node_exporter-1.6.1.linux-amd64.tar.gz
tar xvf node_exporter-1.6.1.linux-amd64.tar.gz
sudo mv node_exporter-1.6.1.linux-amd64/node_exporter /usr/local/bin/
sudo chmod +x /usr/local/bin/node_exporter
rm -rf node_exporter-1.6.1.linux-amd64*
```

### 4. Loki (Log Aggregation)

```bash
wget https://github.com/grafana/loki/releases/download/v3.4.1/loki-linux-amd64.zip
unzip loki-linux-amd64.zip
sudo mv loki-linux-amd64 /usr/local/bin/loki
sudo chmod +x /usr/local/bin/loki
rm loki-linux-amd64.zip
```

### 5. Promtail (Log Shipping)

```bash
wget https://github.com/grafana/loki/releases/download/v2.9.0/promtail-linux-amd64.zip
unzip promtail-linux-amd64.zip
sudo mv promtail-linux-amd64 /usr/local/bin/promtail
sudo chmod +x /usr/local/bin/promtail
rm promtail-linux-amd64.zip
```

---

## Verify Installation

Check that all required binaries are installed:

```bash
which node_exporter
which prometheus
which grafana-server
which loki
which promtail
which node
which pnpm
```

All commands should return a path (e.g., `/usr/local/bin/prometheus`).

Or use the automated verification:
```bash
./install.sh --verify
```

---

## Distribution-Specific Notes

### Ubuntu/Debian
- Most packages available via apt
- Node Exporter: `sudo apt install prometheus-node-exporter`
- Recommended for easiest setup

### Arch Linux
- Use pacman or AUR for most packages
- Example: `sudo pacman -S prometheus grafana`
- Node Exporter: `sudo pacman -S prometheus-node-exporter`

### Fedora/RHEL/CentOS
- Use dnf/yum for most packages
- Example: `sudo dnf install prometheus grafana`
- Node Exporter: `sudo dnf install golang-github-prometheus-node-exporter`

### Other Distributions
- Follow manual installation steps above
- All binaries should be placed in `/usr/local/bin/`
- Ensure binaries are executable: `sudo chmod +x /usr/local/bin/<binary>`

---

## Required Dependencies Summary

| Package | Minimum Version | Purpose |
|---------|----------------|---------|
| Node.js | v18+ | Midgard runtime |
| pnpm | Latest | Package manager |
| Prometheus | 2.x | Metrics database |
| Grafana | 9.x+ | Dashboards |
| Node Exporter | 1.x | System metrics |
| Loki | 2.x+ | Log aggregation |
| Promtail | 2.x+ | Log shipping |

---

## Troubleshooting

### Permission Denied When Moving to /usr/local/bin

Use `sudo`:
```bash
sudo mv <binary> /usr/local/bin/
sudo chmod +x /usr/local/bin/<binary>
```

### wget or unzip Not Found

Install required tools:
```bash
# Ubuntu/Debian
sudo apt install wget unzip

# Arch Linux
sudo pacman -S wget unzip

# Fedora/RHEL
sudo dnf install wget unzip
```

### Node.js Version Too Old

Install newer Node.js:
```bash
# Using NodeSource repository (Ubuntu/Debian)
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt-get install -y nodejs
```

Or download from: https://nodejs.org/

---

## Next Steps

After installation, see:
- **MANUAL-RUN.md** - Manual startup/shutdown instructions

Or simply run:
```bash
./start.sh
```
