# Midgard Local Monitoring Setup

Run Midgard node with full monitoring stack (Prometheus, Grafana, Loki, Promtail) without Docker.

## Prerequisites & Installation

### 1. Install Required Dependencies

#### Node.js & pnpm
```bash
# Node.js v18+ required
node --version

# Install pnpm globally
npm install -g pnpm
```

#### Prometheus
```bash
# macOS
brew install prometheus

# Ubuntu/Debian
sudo apt update
sudo apt install prometheus

# Or download binary manually
wget https://github.com/prometheus/prometheus/releases/download/v2.45.0/prometheus-2.45.0.linux-amd64.tar.gz
tar xvf prometheus-2.45.0.linux-amd64.tar.gz
sudo mv prometheus-2.45.0.linux-amd64/prometheus /usr/local/bin/
sudo chmod +x /usr/local/bin/prometheus
```

#### Grafana
```bash
# Download from: https://grafana.com/grafana/download

# macOS
brew install grafana

# Ubuntu/Debian
sudo apt-get install -y software-properties-common
sudo add-apt-repository "deb https://packages.grafana.com/oss/deb stable main"
wget -q -O - https://packages.grafana.com/gpg.key | sudo apt-key add -
sudo apt-get update
sudo apt-get install grafana
```

#### Node Exporter (for system metrics)
```bash
wget https://github.com/prometheus/node_exporter/releases/download/v1.6.1/node_exporter-1.6.1.linux-amd64.tar.gz
tar xvf node_exporter-1.6.1.linux-amd64.tar.gz
sudo mv node_exporter-1.6.1.linux-amd64/node_exporter /usr/local/bin/
sudo chmod +x /usr/local/bin/node_exporter
```

#### Loki & Promtail (for logs)
```bash
# Download Loki
wget https://github.com/grafana/loki/releases/download/v3.4.1/loki-linux-amd64.zip
unzip loki-linux-amd64.zip
sudo mv loki-linux-amd64 /usr/local/bin/loki
sudo chmod +x /usr/local/bin/loki

# Download Promtail
wget https://github.com/grafana/loki/releases/download/v2.9.0/promtail-linux-amd64.zip
unzip promtail-linux-amd64.zip
sudo mv promtail-linux-amd64 /usr/local/bin/promtail
sudo chmod +x /usr/local/bin/promtail
```

---

## Starting the Monitoring Stack

### Option 1: Automated (Recommended)

```bash
cd /home/g/cardano/midgard/demo/midgard-node/local-monitoring
./start.sh
```

This will:
- Start Node Exporter (system metrics) on port 9100
- Start Loki (log aggregation) on port 3100
- Start Promtail (log shipping) on port 9080
- Start Prometheus (metrics database) on port 9090
- Start Grafana (dashboards) on port 3001
- Build and start Midgard node on ports 3000 (API) and 9464 (metrics)

### Option 2: Manual Steps

#### 1. Start Node Exporter
```bash
cd /home/g/cardano/midgard/demo/midgard-node/local-monitoring
./node_exporter/node_exporter --web.listen-address=:9100 > data/node_exporter.log 2>&1 &
```

#### 2. Start Loki
```bash
./loki/loki -config.file=./loki-config.yaml > data/loki.log 2>&1 &
```

#### 3. Start Promtail
```bash
# Create runtime config
cat > promtail-config-runtime.yaml << EOF
server:
  http_listen_port: 9080
  grpc_listen_port: 0

positions:
  filename: $PWD/data/positions.yaml

clients:
  - url: http://localhost:3100/loki/api/v1/push

scrape_configs:
  - job_name: system
    static_configs:
      - targets:
          - localhost
        labels:
          job: varlogs
          __path__: /var/log/*log
  
  - job_name: midgard
    static_configs:
      - targets:
          - localhost
        labels:
          job: midgard
          __path__: $PWD/data/*.log
EOF

./promtail/promtail -config.file=./promtail-config-runtime.yaml > data/promtail.log 2>&1 &
```

#### 4. Start Prometheus
```bash
mkdir -p data/prometheus-data
prometheus \
  --config.file=./prometheus.yml \
  --storage.tsdb.path=./data/prometheus-data \
  --web.enable-remote-write-receiver \
  --enable-feature=exemplar-storage \
  --enable-feature=native-histograms \
  --web.listen-address=0.0.0.0:9090 \
  > data/prometheus.log 2>&1 &
```

#### 5. Start Grafana
```bash
# Create provisioning directories
mkdir -p grafana-provisioning/datasources
mkdir -p grafana-provisioning/dashboards
mkdir -p data/grafana-data
mkdir -p data/grafana-logs
mkdir -p data/grafana-plugins

# Copy configurations
cp datasources.yml grafana-provisioning/datasources/
cp dashboard.json grafana-provisioning/dashboards/

# Create dashboards provisioning config
cat > grafana-provisioning/dashboards/dashboards.yml << EOF
apiVersion: 1
providers:
  - name: Default
    folder: Services
    type: file
    options:
      path: $PWD/grafana-provisioning/dashboards/dashboard.json
    disableDeletion: false
    editable: true
    updateIntervalSeconds: 10
EOF

# Start Grafana
GF_PATHS_DATA="$PWD/data/grafana-data" \
GF_PATHS_LOGS="$PWD/data/grafana-logs" \
GF_PATHS_PLUGINS="$PWD/data/grafana-plugins" \
GF_PATHS_PROVISIONING="$PWD/grafana-provisioning" \
GF_SERVER_HTTP_PORT=3001 \
GF_AUTH_ANONYMOUS_ENABLED=true \
GF_AUTH_ANONYMOUS_ORG_ROLE=Admin \
grafana-server --homepath=/usr/share/grafana > data/grafana.log 2>&1 &
```

#### 6. Start Midgard Node
```bash
cd ..
pnpm run build
node dist/index.js listen > local-monitoring/data/midgard.log 2>&1 &
```

---

## Accessing Services

| Service | URL | Credentials |
|---------|-----|-------------|
| **Midgard API** | http://localhost:3000 | - |
| **Midgard Metrics** | http://localhost:9464/metrics | - |
| **Prometheus** | http://localhost:9090 | - |
| **Grafana** | http://localhost:3001 | admin / admin |
| **Loki** | http://localhost:3100 | - |
| **Node Exporter** | http://localhost:9100/metrics | - |

### Grafana Dashboard
1. Open http://localhost:3001
2. Login with `admin` / `admin`
3. Navigate to: **Dashboards** → **Services** → **Default**

---

## Stopping the Monitoring Stack

### Option 1: Automated (Recommended)

```bash
cd /home/g/cardano/midgard/demo/midgard-node/local-monitoring
./stop.sh
```

This will gracefully stop all services and clean up PID files.

### Option 2: Manual Steps

#### 1. Stop All Services by PID
```bash
# Find and kill processes
pkill -f "node_exporter --web.listen-address"
pkill -f "loki -config.file"
pkill -f "promtail -config.file"
pkill -f "prometheus --config.file"
pkill -f "grafana-server"
pkill -f "node dist/index.js listen"
```

#### 2. Clean Up
```bash
# Remove runtime configurations
rm -f promtail-config-runtime.yaml

# Remove Grafana provisioning directory (will be recreated on next start)
rm -rf grafana-provisioning
```

#### 3. Verify All Stopped
```bash
# Check if any processes are still running
ps aux | grep -E "prometheus|grafana|loki|promtail|node_exporter|midgard" | grep -v grep
```

---

## Dashboard Metrics

The Grafana dashboard displays:

### Real-Time Metrics (when active)
- **Transactions in Queue** - Current transactions waiting
- **Mempool Transactions** - Transactions in mempool
- **Received Transactions Per Second** - Transaction rate
- **Committed Blocks Per Minute** - Block commitment rate

### Historical Metrics
- **Received Transactions** - Total transactions received
- **Committed Blocks** - Total blocks committed to L1
- **Committed Transactions** - Total transactions in blocks
- **L1 Block Commitment Size** - Size of commitment transactions

### Merge Metrics (requires 8+ blocks in queue)
- **Merged Blocks** - Total blocks merged to confirmed state
- **Merged Blocks Per Minute** - Merge rate

### System Metrics (all services)
- **CPU Usage** - Per-service CPU consumption
- **Memory Usage** - Per-service memory consumption

---

## Troubleshooting

### Port Already in Use
```bash
# Check which process is using a port
lsof -i :3000  # Midgard API
lsof -i :3001  # Grafana
lsof -i :9090  # Prometheus
lsof -i :9464  # Midgard Metrics

# Kill specific process
kill -9 <PID>
```

### No Data in Grafana
1. Verify Prometheus has data: http://localhost:9090
   - Query: `tx_count_total`
   - Should show current transaction count
2. Check time range in Grafana (top-right)
   - Try "Last 6 hours" or "Last 24 hours"
3. Hard refresh browser: `Ctrl+Shift+R` (Windows/Linux) or `Cmd+Shift+R` (Mac)

### Services Not Starting
```bash
# Check logs
tail -f data/prometheus.log
tail -f data/grafana.log
tail -f data/midgard.log

# Verify binaries exist
ls -la node_exporter/node_exporter
ls -la loki/loki
ls -la promtail/promtail
```

### Midgard Build Errors
```bash
# Rebuild from parent directory
cd /home/g/cardano/midgard/demo/midgard-node
pnpm install
pnpm run build
```

---

## File Structure

```
local-monitoring/
├── start.sh                    # Start all services
├── stop.sh                     # Stop all services
├── .gitignore                  # Git ignore rules
├── dashboard.json              # Grafana dashboard definition
├── datasources.yml             # Grafana datasources config
├── dashboards.yml              # Dashboard provisioning config
├── prometheus.yml              # Prometheus scrape config
├── loki-config.yaml           # Loki configuration
├── promtail-config.yaml       # Promtail configuration template
├── node_exporter/             # Node Exporter binary (gitignored)
├── loki/                      # Loki binary (gitignored)
├── promtail/                  # Promtail binary (gitignored)
└── data/                      # All runtime data (gitignored)
    ├── prometheus-data/       # Prometheus TSDB
    ├── grafana-data/          # Grafana database
    ├── grafana-logs/          # Grafana logs
    ├── grafana-plugins/       # Grafana plugins
    └── *.log                  # Service logs
```

---

## Notes

- **First Run**: The `start.sh` script will automatically download binaries for Node Exporter, Loki, and Promtail if they don't exist
- **Data Persistence**: All data is stored in the `data/` directory and persists across restarts
- **Auto-Refresh**: Grafana dashboard auto-refreshes every 10 seconds
- **Log Aggregation**: All service logs are collected by Promtail and available in Grafana via Loki
- **Graceful Shutdown**: Always use `./stop.sh` to ensure proper cleanup of resources

---

## Support

For issues or questions, refer to the main Midgard documentation or check service logs in the `data/` directory.
