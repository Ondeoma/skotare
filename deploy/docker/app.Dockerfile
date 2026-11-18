# skotare アプリ（scripts/dist.sh の成果物を詰めるだけ）
FROM eclipse-temurin:21-jre-alpine
RUN addgroup -S skotare && adduser -S -G skotare skotare
COPY dist/lib /app/lib
COPY dist/static /app/static
ENV STATIC_DIR=/app/static HTTP_PORT=8080
USER skotare
EXPOSE 8080
CMD ["java", "-XX:MaxRAMPercentage=70", "-cp", "/app/lib/*", "skotare.app.Main"]
