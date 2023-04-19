import java.util.*;
import java.util.concurrent.*;

class Colmeia {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int numOperarios = scanner.nextInt();
        int numTarefas = scanner.nextInt();
        scanner.nextLine();

        Map<Integer, Tarefa> tarefas = new HashMap<>(); // Armazena todas as tarefas
        Queue<Tarefa> filaTarefas = new LinkedList<>(); // Fila das tarefas a serem executadas

        // Leitura das tarefas
        for (int i = 0; i < numTarefas; i++) {
            String[] entrada = scanner.nextLine().split(" ");
            int id = Integer.parseInt(entrada[0]);
            int duracao = Integer.parseInt(entrada[1]);

            List<Integer> dependencias = new ArrayList<>();
            for (int j = 2; j < entrada.length; j++) {
                dependencias.add(Integer.parseInt(entrada[j]));
            }

            Tarefa tarefa = new Tarefa(id, duracao, dependencias);
            tarefas.put(id, tarefa);
            filaTarefas.offer(tarefa);
        }

        // Cria um executorService com o tamanho de número de operários
        ExecutorService executorService = Executors.newFixedThreadPool(numOperarios);

        while (!filaTarefas.isEmpty()) {
            Iterator<Tarefa> iterator = filaTarefas.iterator();

            // Itera pela lista de tarefas a serem executadas
            while (iterator.hasNext()) {
                Tarefa tarefa = iterator.next();

                if (tarefa.podeSerExecutada(tarefas)) {
                    executorService.execute(tarefa);
                    iterator.remove();
                }
            }
        }

        executorService.shutdown();
    }

    static class Tarefa implements Runnable {
        int id;
        int duracao;
        List<Integer> dependencias;
        boolean concluida;

        public Tarefa(int id, int duracao, List<Integer> dependencias) {
            this.id = id;
            this.duracao = duracao;
            this.dependencias = dependencias;
            this.concluida = false;
        }

        // Quando uma tarefa é executada, ela é colocada em sleep para simular o tempo de execução da tarefa
        // ao concluir, printa a saída.
        @Override
        public void run() {
            try {
                Thread.sleep(duracao);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            concluida = true;
            System.out.println("tarefa " + id + " feita");
        }

        public boolean podeSerExecutada(Map<Integer, Tarefa> tarefas) {
            for (int dependencia : dependencias) {
                Tarefa tarefa = tarefas.get(dependencia);
                if (tarefa == null || !tarefa.concluida) {
                    return false;
                }
            }
            return true;
        }
    }
}