package au.id.cmears.courant;

import android.app.Notification;
import android.app.PendingIntent;
import android.app.Service;
import android.app.TaskStackBuilder;
import android.content.Context;
import android.content.Intent;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Binder;
import android.os.Bundle;
import android.os.IBinder;
import android.util.Log;

/**
 * Created by chris on 7/07/15.
 */
public class RunService extends Service {
    public int NOTIFICATION_ID = 1;

    Run currentRun;
    LocationManager locMan;
    LocationListener locListener;

    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d("RunService", "onStartCommand");
        // Put this service in the foreground.
        Intent clickIntent = new Intent(this, RunInProgressActivity.class);
        TaskStackBuilder stackBuilder = TaskStackBuilder.create(this);
        stackBuilder.addParentStack(RunInProgressActivity.class);
        stackBuilder.addNextIntent(clickIntent);
        PendingIntent pendingIntent =
                stackBuilder.getPendingIntent(0, PendingIntent.FLAG_UPDATE_CURRENT);
        Notification notification = new Notification.Builder(this)
                .setContentTitle("Run in progress")
                .setContentText("Content text")
                .setContentIntent(pendingIntent)
                .setSmallIcon(R.drawable.ic_launcher)
                .build();
        startForeground(NOTIFICATION_ID, notification);

        // Initialise the run.
        currentRun = new Run();

        // Request location updates.
        locMan = (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);
        locListener = new LocationListener() {
            public void onLocationChanged(Location loc) {
                currentRun.newLocation(loc);
            }
            public void onStatusChanged(String provider, int status, Bundle extras) {}
            public void onProviderEnabled(String provider) {}
            public void onProviderDisabled(String provider) {}
        };
        locMan.requestLocationUpdates(LocationManager.GPS_PROVIDER, 100, 0, locListener);

        return START_STICKY;
    }

    private final IBinder binder = new LocalBinder();

    public class LocalBinder extends Binder {
        RunService getService() {
            return RunService.this;
        }
    }

    public IBinder onBind(Intent intent) {
        return binder;
    }

    public void stopCurrentRun() {
        locMan.removeUpdates(locListener);
    }

    public Run getCurrentRun() {
        return currentRun;
    }

    public void onDestroy() {
        stopCurrentRun();
    }
}
