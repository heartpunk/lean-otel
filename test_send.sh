curl -s -o /dev/null -w "%{http_code}" \
  -X POST https://api.honeycomb.io/v1/traces \
  -H "Content-Type: application/json" \
  -H "x-honeycomb-team: zoFbjFUA5ErGjhw9T2CtWC" \
  -d '{
    "resourceSpans": [{
      "resource": {"attributes": [{"key": "service.name", "value": {"stringValue": "lean-otel-test"}}]},
      "scopeSpans": [{
        "scope": {"name": "lean-otel"},
        "spans": [{
          "traceId": "0af7651916cd43dd8448eb211c80319c",
          "spanId": "b7ad6b7169203331",
          "name": "test-span",
          "startTimeUnixNano": "1711900000000000000",
          "endTimeUnixNano": "1711900001000000000",
          "attributes": [{"key": "test", "value": {"stringValue": "hello from lean-otel"}}]
        }]
      }]
    }]
  }'
