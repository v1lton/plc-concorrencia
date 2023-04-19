import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.Scanner;
import java.util.Random;

public class Main {
    private int numPistas;
    private final Lock lock = new ReentrantLock();
    private final Condition[] pistas;
    private final int[] numAeronavesAguardando;
    long horarioInicial = System.currentTimeMillis();

    public Main(int numPistas) {
        this.numPistas = numPistas;
        this.pistas = new Condition[numPistas];
        this.numAeronavesAguardando = new int[numPistas];
        for (int i = 0; i < numPistas; i++) {
            pistas[i] = lock.newCondition();
            numAeronavesAguardando[i] = 0;
        }
    }

    public void decolar(int numAviao, long horarioEsperado) throws InterruptedException {
        lock.lock();
        try {
            while (numPistas == 0) {
                for (Condition pista : pistas) {
                    pista.await();
                }
            }

            int pista = buscarPistaDisponivel();
            numPistas--;

            long horarioReal = System.currentTimeMillis() - horarioInicial;
            long atraso = horarioReal - horarioEsperado;
            System.out.printf("Avião %d decolou às %d (esperado: %d, atraso: %d)\n",
                    numAviao + 1, horarioReal, horarioEsperado, atraso);
            Thread.sleep(500);

            numPistas++;
            pistas[pista].signalAll();
        } finally {
            lock.unlock();
        }
    }

    public void aterrissar(int numAviao, long horarioEsperado) throws InterruptedException {
        lock.lock();
        try {
            while (numPistas == 0) {
                for (Condition pista : pistas) {
                    pista.await();
                }
            }

            int pista = buscarPistaDisponivel();
            numPistas--;

            long horarioReal = System.currentTimeMillis() - horarioInicial;
            long atraso = horarioReal - horarioEsperado;
            System.out.printf("Avião %d aterrissou às %d (esperado: %d, atraso: %d)\n",
                    numAviao + 1, horarioReal, horarioEsperado, atraso);
            Thread.sleep(500);

            numPistas++;
            pistas[pista].signalAll();
        } finally {
            lock.unlock();
        }
    }

    private int buscarPistaDisponivel() {
        int pista = 0;
        for (int i = 1; i < numPistas; i++) {
            if (numAeronavesAguardando[i] < numAeronavesAguardando[pista]) {
                pista = i;
            }
        }
        return pista;
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n, m, k;

        System.out.print("Quantidade de aviões esperando para decolar (N): ");
        n = scanner.nextInt();
        int[] horariosDecolagem = new int[n];
        for (int i = 0; i < n; i++) {
            System.out.printf("Horário de saída do avião %d: ", i + 1);
            horariosDecolagem[i] = scanner.nextInt();
        }

        System.out.print("\nQuantidade de aviões esperando para aterrissar (M): ");
        m = scanner.nextInt();
        int[] horariosAterrissagem = new int[m];
        for (int i = 0; i < m; i++) {
            System.out.printf("Horário de chegada do avião %d: ", i + 1);
            horariosAterrissagem[i] = scanner.nextInt();
        }

        System.out.print("\nNúmero de pistas disponíveis (K): ");
        k = scanner.nextInt();

        Main aeroporto = new Main(k);

        (new Thread(new Decolar(aeroporto, horariosDecolagem, n))).start();
        (new Thread(new Aterrisar(aeroporto, horariosAterrissagem, m))).start();
    }

    public static class Decolar implements Runnable {
        private Main aeroporto;
        private int[] horarioDecolagem;
        private int n;
        private Random random = new Random();

        public Decolar(Main a, int[] h, int n) {
            this.aeroporto = a;
            this.horarioDecolagem = h;
            this.n = n;
        }

        public void run() {
            for (int i = 0; i < n; i++) {
                try {
                    aeroporto.decolar(i, horarioDecolagem[i]);
                    Thread.sleep(random.nextInt(5) * 1000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    public static class Aterrisar implements Runnable {
        private Main aeroporto;
        private int[] horarioAterrissagem;
        private int m;
        private Random random = new Random();

        public Aterrisar(Main a, int[] h, int m) {
            this.aeroporto = a;
            this.horarioAterrissagem = h;
            this.m = m;
        }

        public void run() {
            for (int i = 0; i < m; i++) {
                try {
                    aeroporto.aterrissar(i, horarioAterrissagem[i]);
                    Thread.sleep(random.nextInt(5) * 1000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}