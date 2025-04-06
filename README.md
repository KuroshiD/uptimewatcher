# UptimeWatcher

UptimeWatcher é uma aplicação Haskell para monitoramento de URLs. Ele verifica periodicamente se os sites estão online ou offline e gera logs detalhados com os resultados. Além disso, permite realizar scans manuais e configurar intervalos de monitoramento.

## Funcionalidades

- **Monitoramento Automático**: Verifica periodicamente o status das URLs configuradas.
- **Scan Manual**: Permite realizar verificações manuais a qualquer momento.
- **Configuração de Intervalo**: Ajuste o intervalo entre os scans automáticos.
- **Logs Detalhados**: Gera logs com o status das URLs e alertas para sites offline.
- **Gerenciamento de URLs**: Adicione ou remova URLs da lista de monitoramento.

## Instalação

1. Certifique-se de que você possui o [Stack](https://docs.haskellstack.org/en/stable/README/) instalado.
2. Clone este repositório:
   ```bash
   git clone https://github.com/KuroshiD/uptimewatcher.git
   cd uptimewatcher
   ```
3. Compile o projeto:
   ```bash
   stack build
   ```

## Uso

### Executando o Aplicativo

Para iniciar o UptimeWatcher, execute:
```bash
stack run
```

### Menu Interativo

Após iniciar o aplicativo, você verá o seguinte menu:

```
UptimeWatcher Menu:
1. Add URL
2. Remove URL
3. List URLs
4. Run manual scan
5. Scan specific URL
6. Set scan interval (in minutes)
7. Clear screen
8. Exit
Choose an option:
```

### Casos de Uso

#### 1. Adicionar uma URL
Escolha a opção `1` no menu e insira a URL que deseja monitorar. Exemplo:
```
Enter URL to add: http://example.com
URL added.
```

#### 2. Remover uma URL
Escolha a opção `2` no menu e insira a URL que deseja remover. Exemplo:
```
Enter URL to remove: http://example.com
URL removed.
```

#### 3. Listar URLs Monitoradas
Escolha a opção `3` para visualizar todas as URLs atualmente monitoradas:
```
Monitored URLs:
http://example.com
http://nonexistent.url
```

#### 4. Executar um Scan Manual
Escolha a opção `4` para realizar um scan manual de todas as URLs monitoradas. O status será exibido no terminal e registrado no log:
```
Running manual scan...
http://example.com is online.
http://nonexistent.url is offline.
Scan completed.
```

#### 5. Scan de uma URL Específica
Escolha a opção `5` e insira a URL que deseja verificar:
```
Enter URL to scan: http://example.com
Scanning http://example.com...
http://example.com is online.
```

#### 6. Configurar Intervalo de Scan
Escolha a opção `6` para ajustar o intervalo entre os scans automáticos. Insira o intervalo em minutos:
```
Current scan interval: 30 minutes
Enter new scan interval in minutes: 10
Scan interval updated. Restarting timer...
```

#### 7. Limpar a Tela
Escolha a opção `7` para limpar a tela do terminal.

#### 8. Sair
Escolha a opção `8` para encerrar o aplicativo.

## Logs

Os resultados dos scans são registrados no arquivo `scan.log`. Exemplo de log:
```
Scan at 2025-04-06 00:36:00:
Monitored URLs:
http://example.com online
http://nonexistent.url offline
Alerts:
http://nonexistent.url IS OFFLINE IN 3 CONSECUTIVE SCANS
-----------------------------
```

## Contribuição

Contribuições são bem-vindas! Sinta-se à vontade para abrir issues ou enviar pull requests.

## Licença

Este projeto está licenciado sob a licença BSD-3-Clause. Consulte o arquivo [LICENSE](./LICENSE) para mais detalhes.
