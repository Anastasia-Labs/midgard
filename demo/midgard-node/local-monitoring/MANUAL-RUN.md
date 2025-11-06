# Manual Run Guide - Midgard Local Monitoring

This guide explains how to manually start and stop all monitoring services without using the automated scripts.

## Prerequisites

Ensure all dependencies are installed. See **INSTALL.md** or run `./install.sh`.

---

## Manual Startup

### 1. Create Data Directories

```bash
cd /path/to/midgard/demo/midgard-node/local-monitoring
mkdir -p data/prometheus-data
mkdir -p data/grafana-data
mkdir -p data/grafana-logs
mkdir -p data/grafana-plugins
```

### 2. Start Node Exporter

```bash
node_exporter --web.listen-address=:9100 > data/node_exporter.log 2>&1 &
echo "Node Exporter started (PID: $!)"
```

### 3. Start Loki

```bash
loki -config.file=./loki-config.yaml > data/loki.log 2>&1 &
echo "Loki started (PID: $!)"
```

### 4. Start Promtail

```bash
# Create runtime config with actual paths
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

promtail -config.file=./promtail-config-runtime.yaml > data/promtail.log 2>&1 &
echo "Promtail started (PID: $!)"
```

### 5. Start Prometheus

```bash
prometheus \
  --config.file=./prometheus.yml \
  --storage.tsdb.path=./data/prometheus-data \
  --web.enable-remote-write-receiver \
  --enable-feature=exemplar-storage \
  --enable-feature=native-histograms \
  --web.listen-address=0.0.0.0:9090 \
  > data/prometheus.log 2>&1 &
echo "Prometheus started (PID: $!)"
```

### 6. Start Grafana

```bash
# Create provisioning directories
mkdir -p grafana-provisioning/datasources
mkdir -p grafana-provisioning/dashboards

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

# Start Grafana with environment variables
GF_PATHS_DATA="$PWD/data/grafana-data" \
GF_PATHS_LOGS="$PWD/data/grafana-logs" \
GF_PATHS_PLUGINS="$PWD/data/grafana-plugins" \
GF_PATHS_PROVISIONING="$PWD/grafana-provisioning" \
GF_SERVER_HTTP_PORT=3001 \
GF_AUTH_ANONYMOUS_ENABLED=true \
GF_AUTH_ANONYMOUS_ORG_ROLE=Admin \
GF_FEATURE_TOGGLES_ENABLE="traceqlEditor metricsSummary" \
grafana-server \
  --homepath=/usr/share/grafana \
  > data/grafana.log 2>&1 &
echo "Grafana started (PID: $!)"
```

### 7. Start Midgard Node

```bash
# Navigate to Midgard directory
cd ../

# Build Midgard
pnpm run build

# Start Midgard
node dist/index.js listen > local-monitoring/data/midgard.log 2>&1 &
echo "Midgard started (PID: $!)"

# Return to monitoring directory
cd local-monitoring/
```

---

## Accessing Services

| Service | URL | Credentials |
|---------|-----|-------------|
| **Midgard API** | http://localhost:3000 | - |
| **Midgard Metrics** | http://localhost:9464/metrics | - |
| **Midgard Process Metrics** | http://localhost:3000/process-metrics | - |
| **Prometheus** | http://localhost:9090 | - |
| **Grafana** | http://localhost:3001 | admin / admin |
| **Loki** | http://localhost:3100 | - |
| **Node Exporter** | http://localhost:9100/metrics | - |
| **Promtail** | http://localhost:9080 | - |

### Grafana Dashboard

1. Open http://localhost:3001
2. Login with `admin` / `admin`
3. Navigate to: **Dashboards** → **Services** → **Default**

---

## Manual Shutdown

### Option 1: Stop All Services by Name

```bash
# Stop each service
pkill -f "node_exporter --web.listen-address"
pkill -f "loki -config.file"
pkill -f "promtail -config.file"
pkill -f "prometheus --config.file"
pkill -f "grafana-server"
pkill -f "node dist/index.js listen"

echo "All services stopped"
```

### Option 2: Stop by PID (if you tracked them)

If you saved PIDs during startup:

```bash
# Kill specific processes
kill <node_exporter_pid>
kill <loki_pid>
kill <promtail_pid>
kill <prometheus_pid>
kill <grafana_pid>
kill <midgard_pid>
```

### Clean Up Runtime Files

```bash
# Remove runtime configurations
rm -f promtail-config-runtime.yaml

# Remove Grafana provisioning (will be recreated on next start)
rm -rf grafana-provisioning
```

### Verify All Stopped

```bash
ps aux | grep -E "prometheus|grafana|loki|promtail|node_exporter|midgard" | grep -v grep
```

Should return no results if all services are stopped.

---

## Viewing Logs

All service logs are stored in the `data/` directory:

```bash
# Midgard logs
tail -f data/midgard.log

# Prometheus logs
tail -f data/prometheus.log

# Grafana logs
tail -f data/grafana.log

# Loki logs
tail -f data/loki.log

# Promtail logs
tail -f data/promtail.log

# Node Exporter logs
tail -f data/node_exporter.log
```

---

## Troubleshooting

### Port Already in Use

Check which process is using a port:
```bash
lsof -i :3000  # Midgard API
lsof -i :3001  # Grafana
lsof -i :9090  # Prometheus
lsof -i :9464  # Midgard Metrics
```

Kill the process:
```bash
kill -9 <PID>
```

### Service Not Starting

Check the log file for errors:
```bash
tail -50 data/<service>.log
```

### No Data in Grafana

1. Verify Prometheus has data: http://localhost:9090
   - Query: `tx_count_total`
2. Check time range in Grafana (top-right)
   - Try "Last 6 hours" or "Last 24 hours"
3. Hard refresh browser: `Ctrl+Shift+R` (Windows/Linux) or `Cmd+Shift+R` (Mac)

### Midgard Build Errors

```bash
cd ../
pnpm install
pnpm run build
```

---

## Automated Alternative

Instead of manual steps, you can use the automated scripts:

```bash
# Start all services
./start.sh

# Stop all services
./stop.sh
```

See **README.md** for more information.


