package au.id.cmears.courant;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;


public class RunInProgressActivity extends Activity implements ServiceConnection {

    RunService runService;
    Run run;
    Handler handler;
    boolean tryToUpdate;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_run_in_progress);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_run_in_progress, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    public void onStart() {
        // Whenever the activity is awake, bind to the RunService.
        Intent intent = new Intent(this, RunService.class);
        bindService(intent, this, 0);
        tryToUpdate = true;
        handler = new Handler();
        handler.postDelayed(new UpdateRun(), 1000);
        super.onStart();
    }

    private class UpdateRun implements Runnable {
        public void run() {
            if (tryToUpdate) {
                Log.d("RIPA", "UpdateRun:run");
                updateRun();
                handler.postDelayed(this, 1000);
            }
        }
    }

    public void onStop() {
        unbindService(this);
        super.onStop();
    }

    public void stopRun(View view) {
        updateRun();
        tryToUpdate = false;
        recordRun();
        stopService(new Intent(this, RunService.class));
        finish();
    }

    public void updateRun() {
        if (runService != null) {
            run = runService.getCurrentRun();
            Log.d("RIPA", "current run: " + run.toString());
            Log.d("RIPA", "json: " + run.toJSON());
            TextView info = (TextView) findViewById(R.id.currentRunInfo);
            int numSamples = run.numSamples();
            info.setText(Integer.toString(numSamples));
        } else {
            Log.d("RIPA", "not bound");
        }
    }

    public void recordRun() {
        if (run != null) {
            RunStorage.recordRun(this, run);
        }
    }

    public void onServiceConnected(ComponentName name, IBinder service) {
        Log.d("RIPA", "service has been bound");
        RunService.LocalBinder binder = (RunService.LocalBinder) service;
        runService = binder.getService();
    }

    public void onServiceDisconnected(ComponentName name) {
        runService = null;
    }
}