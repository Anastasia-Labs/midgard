# Manual Run Guide - Midgard Local Run

This guide explains how to manually start and stop all services without using the automated scripts.

## Prerequisites

Ensure all dependencies are installed. See **INSTALL.md** or run:
```bash
./install.sh
```

---

## Manual Startup

### 1. Create Data Directories

```bash
cd /path/to/midgard/demo/midgard-node/local-run
mkdir -p data
```

### 2. Start Node Exporter (System Metrics)

```bash
node_exporter --web.listen-address=:9100 > data/node_exporter.log 2>&1 &
```

### 3. Start Loki (Log Aggregation)

```bash
loki -config.file=./loki-config.yaml > data/loki.log 2>&1 &
```

### 4. Start Promtail (Log Shipping)

```bash
promtail -config.file=./promtail-config.yaml > data/promtail.log 2>&1 &
```

### 5. Start Prometheus (Metrics Database)

```bash
prometheus \
  --config.file=./prometheus.yml \
  --storage.tsdb.path=./data/prometheus-data \
  --web.enable-remote-write-receiver \
  --enable-feature=exemplar-storage \
  --enable-feature=native-histograms \
  --web.listen-address=0.0.0.0:9090 \
  > data/prometheus.log 2>&1 &
```

### 6. Start Grafana (Dashboards)

```bash
# Create provisioning directories
mkdir -p grafana-provisioning/datasources
mkdir -p grafana-provisioning/dashboards

# Copy datasource configuration
cp datasources.yml grafana-provisioning/datasources/

# Copy dashboard file
cp dashboard.json grafana-provisioning/dashboards/

# Create dashboard provisioning config
cat > grafana-provisioning/dashboards/dashboards.yml << EOF
apiVersion: 1
providers:
  - name: Default
    folder: Services
    type: file
    options:
      path: $(pwd)/grafana-provisioning/dashboards/dashboard.json
    disableDeletion: false
    editable: true
    updateIntervalSeconds: 10
EOF

# Start Grafana
GF_PATHS_DATA="$(pwd)/data/grafana-data" \
GF_PATHS_LOGS="$(pwd)/data/grafana-logs" \
GF_PATHS_PLUGINS="$(pwd)/data/grafana-plugins" \
GF_PATHS_PROVISIONING="$(pwd)/grafana-provisioning" \
GF_SERVER_HTTP_PORT=3001 \
GF_AUTH_ANONYMOUS_ENABLED=true \
GF_AUTH_ANONYMOUS_ORG_ROLE=Admin \
GF_FEATURE_TOGGLES_ENABLE="traceqlEditor metricsSummary" \
grafana-server \
  --homepath=/usr/share/grafana \
  > data/grafana.log 2>&1 &
```

**Note:** `--homepath` should point to your Grafana installation directory. Common locations:
- Ubuntu/Debian: `/usr/share/grafana`
- Arch: `/usr/share/grafana`
- Manual install: adjust accordingly

### 7. Start Midgard Node

```bash
# Navigate to Midgard directory (from local-run)
cd ..

# Start Midgard
pnpm run listen > local-run/data/midgard.log 2>&1 &

# Return to local-run directory
cd local-run
```

---

## Accessing Services

| Service             | URL                                   | Purpose                              |
|---------------------|---------------------------------------|--------------------------------------|
| **Midgard API**     | http://localhost:3000                 | Tx submission, address history, etc. |
| **Grafana**         | http://localhost:3001                 | View dashboards & logs               |
| **Prometheus**      | http://localhost:9090                 | Query metrics                        |
| **Midgard Metrics** | http://localhost:9464/metrics         | L2 metrics (OpenTelemetry)           |
| **Process Metrics** | http://localhost:3000/process-metrics | CPU/Memory metrics                   |
| **Node Exporter**   | http://localhost:9100/metrics         | System metrics                       |
| **Loki**            | http://localhost:3100                 | Logs API                             |

### Grafana Dashboard

1. Open http://localhost:3001
2. Login: `admin` / `admin`
3. Navigate: **Dashboards** → **Services** → **Default**

---

## Manual Shutdown

### Stop All Services

```bash
pkill -f "node_exporter --web.listen-address"
pkill -f "loki -config.file"
pkill -f "promtail -config.file"
pkill -f "prometheus --config.file"
pkill -f "grafana-server"
pkill -f "pnpm run listen"
```

### Clean Up Runtime Files

```bash
rm -rf grafana-provisioning
```

### Verify Shutdown

```bash
ps aux | grep -E "prometheus|grafana|loki|promtail|node_exporter|midgard" | grep -v grep
```

Should return nothing if all services stopped.

---

## Viewing Logs

All logs are in the `data/` directory:

```bash
# Follow Midgard logs
tail -f data/midgard.log

# Follow all logs
tail -f data/*.log
```

**Or view in Grafana:**
1. Open http://localhost:3001
2. Go to **Explore**
3. Select **Loki** datasource
4. Query: `{job="monitoring"}`

---

## Troubleshooting

### Port Already in Use

```bash
# Check what's using the port
lsof -i :3001

# Kill it
kill -9 <PID>
```

### Service Won't Start

Check the log:
```bash
tail -50 data/<service>.log
```

### No Data in Grafana

1. Check Prometheus has data: http://localhost:9090/targets
   - All targets should be "UP"
2. Check time range in Grafana (top-right)
   - Try "Last 6 hours"
3. Hard refresh: `Ctrl+Shift+R`

### Grafana "homepath" Error

If Grafana fails with a homepath error, find your Grafana installation:

```bash
# Find Grafana directory
dpkg -L grafana | grep share/grafana$ # Debian/Ubuntu
pacman -Ql grafana | grep share/grafana$ # Arch
rpm -ql grafana | grep share/grafana$ # Fedora/RHEL
```

Then update the `--homepath` flag accordingly.

---

## Automated Alternative

For easier management, use the automated scripts:

```bash
# Start everything
./start.sh

# Stop everything
./stop.sh
```

The automated script handles:
- ✓ Port conflict detection
- ✓ Service dependency order
- ✓ Health checks
- ✓ PID tracking
- ✓ Clean shutdown
